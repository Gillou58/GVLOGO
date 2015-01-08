{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Noyau de GVLOGO                         |
  |                  Unité : GVKernel.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVKERNEL - part of GVLOGO
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

unit GVKernel;

//
// Le noyau permet de gérer l'espace mémoire dédié à GVLOGO. En particulier,
// sont traités les variables, les procédures, les primitives, les paquets, les
// listes de propriétés, les propriétés et divers objets.
//
// Le noyau est un des éléments fondamentaux
// de l'interpréteur de GVLOGO.

interface

uses
  Classes, SysUtils,
  GVConsts, // constantes
  GVWords, // mots
  GVPropLists, // listes de propriétés
  GVLists, // listes
  GVErrConsts, // constantes des erreurs
  GVPrimConsts, // constantes des primitives
  GVErrors; // traitement des erreurs

type

  // *** classe pour le noyau

  { TGVLogoKernel }

  TGVLogoKernel = class(TObject)
  strict private
    fError: TGVErrors; // enregistrement d'une erreur
    fProtected: Boolean; // drapeau de protection
    fWorkZone: TGVPropList; // zone de travail général
    fOnChange: TNotifyEvent; // notification des changements
    fTempList: TGVListUtils; // liste de travail
    procedure SetProtected(AValue: Boolean); // protection des données
    // paramètre valide ?
    function IsValidParam(const Name: string): Boolean;
  protected
    // gestion des changements
    procedure Change;
    // est-ce un type d'objet donné ?
    function IsObj(const Name, Kind: string): Boolean;
    // compte d'objets d'un type donné
    function CountObj(const Kind: string): Integer;
    // ajoute un objet de type donné
    function AddObj(const Name, Kind, Value: string): TGVError;
    // supprime un objet de type donné
    function RemoveObj(const Name, Kind: string): TGVError;
    // supprime une liste d'objets de type donné
    function RemoveSomeObjs(const L, Kind: string): TGVError;
    // sauvegarde générique
    function Save(const FileName, Lst, Kind: string): TGVError;
    // chargement générique
    function Load(const FileName, Kind: string): TGVError;
    // supprime tous les objets d'un certain type
    procedure RemoveAllObjs(const Kind: string);
    // liste d'objets d'un type donné
    function ObjsToList(const Kind: string): string;
  public
    // *** gestion ***
    // création
    constructor Create;
    // destruction
    destructor Destroy; override;
    // remise à zéro de l'espace de travail
    procedure Clear;
    // renvoie le nombre d'objets
    function Count: Integer;
    // renvoi de tout l'espace de travail
    procedure Dump(Lst: TStrings);
    // l'objet existe-t-il ?
    function Exists(const Name: string): Boolean;
    // l'objet est-il protégé ?
    function IsProtected(const Name: string): Boolean;
    // renvoie les objets de l'espace de travail
    function ToList: string;
    // charge tout l'espace de travail
    function LoadAll(const FileName: string): Boolean;
    // sauvegarde tout l'espace de travail
    function SaveAll(const FileName: string): Boolean;
    // *** traitement des variables ***
    // est-ce une variable?
    function IsVar(const Name: string): Boolean;
    // nombre de variables
    function VarsCount: Integer;
    // affectation à une variable
    function AddVar(const Name, Value: string): Boolean;
    // valeur d'une variable
    function ValVar(const Name: string; out Value: string): Boolean; overload;
    function ValVar(const Name: string): string; overload;
    // destruction d'une variable
    function RemoveVar(const Name: string): Boolean;
    // destruction d'une liste de variables
    function RemoveSomeVars(const Lst: string): Boolean;
    // destruction de toutes les variables
    function RemoveAllVars: Boolean;
    // liste des variables
    function VarsToList: string;
    // chargement d'un fichier de variables
    function LoadVars(const FileName: string): Boolean;
    // sauvegarde d'un fichier de variables
    function SaveVars(const FileName, Lst: string): Boolean;
    // sauvegarde de toutes les variables
    function SaveAllVars(const FileName: string): Boolean;
    // *** traitement des procédures ***
    // est-ce une procédure ?
    function IsProc(const Name: string): Boolean;
    // renvoie le nombre de procédures enregistrées
    function ProcsCount: Integer;
    // renvoie la liste des procédures
    function ProcsToList: string;
    // enregistre une procédure
    function AddProc(const Name, Lst: string): Boolean;
    // supprime une procédure
    function RemoveProc(const Name: string): Boolean;
    // destruction d'une liste de procédures
    function RemoveSomeProcs(const Lst: string): Boolean;
    // supprime toutes les procédures
    function RemoveAllProcs: Boolean;
    // renvoie le nombre de paramètres d'une procédure
    function ParamsCount(const Name: string): Integer;
    // renvoie la liste des paramètres
    function ParamsLine(const Name: string): string; overload;
    function ParamsLine(const Name: string; out ParLine: string)
      : Boolean; overload;
    // renvoie un paramètre par son numéro
    function ParamNum(const Name: string; Num: Integer): string; overload;
    function ParamNum(const Name: string; Num: Integer; out ParNum: string)
      : Boolean; overload;
    // renvoie le nombre de lignes du corps d'une procédure
    function ProcLinesCount(const Name: string): Integer;
    // renvoie une ligne du corps de la procédure spécifiée
    function ProcLine(const Name: string; Line: Integer): string; overload;
    function ProcLine(const Name: string; Line: Integer; out PrLine: string)
      : Boolean; overload;
    // renvoie la définition d'une procédure
    function ProcListDef(const Name: string): string; overload;
    function ProcListDef(const Name: string; out PrListDef: string)
      : Boolean; overload;
    // envoie la procédure vers un éditeur
    function ProcToEdit(const Name: string; Lst: TStrings): Boolean;
    // envoie une liste de procédures vers l'éditeur
    function ProcsToEdit(const LstP: string; Lst: TStrings): Boolean;
    // envoie toutes les procédures vers l'éditeur
    function AllProcsToEdit(Lst: TStrings): Boolean;
    // envoie d'un éditeur vers des procédures
    function EditToProc(Editor: TStrings; FromLine, ToLine: Integer;
      out Err: Integer): Boolean;
    // charge des procédures
    function LoadProcs(const FileName: string): Boolean;
    // sauve des procédures
    function SaveProcs(const FileName, Lst: string): Boolean;
    // sauvegarde toutes les procédures
    function SaveAllProcs(const FileName: string): Boolean;
    // définition valide ?
    function IsValidDef(const LSt: string): Boolean;
    // copie d'une procédure dans une autre
    function CopyDef(const FromProc, ToProc: string): Boolean;
    // *** traitement des primitives ***
    // l'objet est-il une primitive ?
    function IsPrim(const Name: string): Boolean;
    // nombre de primitives
    function PrimsCount: Integer; inline;
    // numéro de primitive
    function NumPrim(const Name: string): Integer;
    // nombre de paramètres d'une primitive
    function NumParamsPrim(const Name: string): Integer;
    // liste des primitives
    function PrimsToList: string;
    // primitive par numéro
    function PrimByNum(const N: Integer): string; overload;
    function PrimByNum(const N: Integer; out PrimBNum: string)
      : Boolean; overload;
    // *** traitement des paquets ***
    // l'objet est-il un paquet ?
    function IsPck(const Name: string): Boolean;
    // renvoie le nombre de paquets
    function PcksCount: Integer;
    // le paquet est-il enfoui ?
    function IsBurriedPck(const Name: string): Boolean;
    // enfouit un paquet
    function BurryPck(const Name: string): Boolean;
    // déterre un paquet
    function UnBurryPck(const Name: string): Boolean;
    // empaquette un objet
    function ToPck(const Name, Obj: string): Boolean;
    // empaquette une liste
    function ListToPck(const Name, Lst: string): Boolean;
    // renvoie la liste des objets d'un paquet
    function PckToList(const Name: string): string;
    // renvoie le nombre d'éléments d'un paquet
    function CountItemsPck(const Name: string): Integer;
    // renvoie la liste des paquets
    function PcksToList: string;
    // crée d'un paquet
    function CreatePck(const Name: string): Boolean;
    // détruit un paquet
    function RemovePck(const Name: string): Boolean;
    // sauvegarde un paquet
    function SavePck(const Name: string): Boolean;
    // paquet vers éditeur
    function PckToEdit(const Name: string; Lst: TStrings): Boolean;
    // dépaquette un objet
    function UnPackObj(const Name: string): Boolean;
    // l'objet appartient-il à un paquet ?
    function IsInPck(const Name: string): Boolean;
    // l'objet est-il enterré ?
    function IsBurried(const Name: string): Boolean;
    // à quel paquet appartient un objet ?
    function BelongsTo(const Name: string): string; overload;
    function BelongsTo(const Name: string; out Which: string): Boolean;
      overload;
    // *** listes de propriétés ***
    // définition d'une propriété
    function DProp(const Name, Prop, Value: string): Boolean;
    // valeur d'une propriété
    function RProp(const Name, Prop: string): string; overload;
    function RProp(const Name, Prop: string; out Value: string)
      : Boolean; overload;
    // annulation d'une propriété
    function AnProp(const Name, Prop: string): Boolean;
    // liste associée à une propriété
    function PListe(const Name: string): string; overload;
    function PListe(const Name: string; out Value: string): Boolean; overload;
    // liste des objets contenant la propriété
    function GList(const Prop: string): string;
    // la liste de propriétés est-elle la liste vide ?
    function IsEmptyPList(const Name: string): Boolean;
    // la propriété est-elle dans la liste de propriétés ?
    function IsProp(const Name, Prop: string): Boolean;
    // la liste de propriétés existe-t-elle ?
    function IsListP(const Name: string): Boolean;
    // *** propriétés ***
    // protection ?
    property Protect: Boolean read fProtected write SetProtected
      default False;
    // événement si changement
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
  end;

implementation

uses
  StrUtils, Forms;

{ TGVLogoKernel }

procedure TGVLogoKernel.SetProtected(AValue: Boolean);
// *** protection ? ***
begin
  if fProtected = AValue then
    Exit; // on sort si aucun changement
  fProtected := AValue; // nouvelle valeur de protection
  Change; // on signifie le changement
end;

procedure TGVLogoKernel.Change;
// *** changement dans le noyau ***
begin
  if Assigned(fOnChange) then // si le gestionnaire existe
    fOnChange(Self); // on l'exécute
end;

function TGVLogoKernel.Exists(const Name: string): Boolean;
// *** l'objet existe-t-il ? ***
begin
  Result := fWorkZone.IsListP(Name); // liste présente ?
end;

function TGVLogoKernel.IsProtected(const Name: string): Boolean;
// *** élément protégé ? ***
begin
  Result := False; // suppose que l'objet n'est pas protégé
  if Exists(Name) then // élément existant ?
  begin
    // si oui, est-ce un paquet ou une primitive ?
    if IsPck(Name) or IsPrim(Name) then
      Result := True // un paquet ou une primitive sont toujours protégés
    else // sinon chercher si présent dans un paquet
    begin
      if IsInPck(Name) then
        // quel paquet ? quelle protection ?
        Result := IsBurriedPck(fWorkZone.ValProp(Name, CInPackage))
      else
        Result := Protect; // sinon protection courante
    end;
  end;
end;

function TGVLogoKernel.ToList: string;
// *** renvoie tous les éléments du noyau ***
begin
  Result := ObjsToList(CExtLP);
end;

function TGVLogoKernel.LoadAll(const FileName: string): Boolean;
// *** chargement d'un espace ***
var
  LErr: TGVError;
begin
  Result := False; // erreur par défaut
  // on charge en écrasant si nécessaire
  LErr := Load(FileName, CExtLP);
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: valeur rendue par Load ###]
    Error.SetError(LErr, FileName);
end;

function TGVLogoKernel.SaveAll(const FileName: string): Boolean;
// *** sauvegarde d'un espace ***
var
  LErr: TGVError;
begin
  Result := False; // erreur par défaut
  // on sauvegarde
  LErr := Save(FileName, ToList, CExtLP);
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: valeur rendue par Save ###]
    Error.SetError(LErr, FileName);
end;

function TGVLogoKernel.IsObj(const Name, Kind: string): Boolean;
// *** est-ce un objet du type donné ? ***
begin
  Result := fWorkZone.IsProp(Name, Kind);
end;

function TGVLogoKernel.CountObj(const Kind: string): Integer;
// *** compte des objets d'un type donné ***
var
  LS: string;
begin
  Result := 0; // pas d'objets
  for LS in fWorkZone do // on balaie la zone de travail
    if (LS <> EmptyStr) and // nom de la liste non vide
      // si c'est un objet du même type (CExtLP ne filtre rien)
      ((Kind = CExtLP) or fWorkZone.IsProp(LS, Kind)) then
        Inc(Result); // on incrémente le résultat
end;

function TGVLogoKernel.AddObj(const Name, Kind, Value: string): TGVError;
// *** ajout d'un objet (avec mise à jour) ***
var
  LWord: TGVWord;
begin
  if IsPrim(Name) then // est-ce une primitive ?
  begin
    // [### Erreur: une primitive ne peut pas être modifiée ###]
    Result := CE_CantModifyPrim;
    Exit; // on sort
  end;
  LWord := TGVWord.Create; // mot de travail créé
  try
    Result := CE_None; // on suppose qu'il n'y a pas d'erreur
    if Exists(Name) then // le nom est-il pris ?
    begin
      if IsObj(Name, Kind) then // est-ce un objet de même nature ?
      begin
        if IsProtected(Name) then // protégé ?
          // [### Erreur : l'objet est protégé ###]
          Result := CE_Protected
        else
        begin
          // sinon mettre à jour
          if fWorkZone.UpDateListP(Name, Kind, Value) then
            Change // notifie le changement
          else
            // [### Erreur: mauvaise liste ###]
            Result := CE_UnknownListWord;
        end;
      end
      else
        // [### Erreur : l'objet est d'une autre nature ###]
        Result := CE_BadObj;
    end
    else
    begin // le nom n'est pas pris
      LWord.Text := Name; // mot normalisé
      if LWord.IsValidIdent then // le nom est-il correct ?
      begin
        // sinon mettre à jour
        if fWorkZone.UpDateListP(Name, Kind, Value) then
          Change // notifie le changement
        else
          // [### Erreur: mauvaise liste ###]
          Result := CE_UnknownListWord;
      end
      else
        // [### Erreur: nom incorrect ###]
        Result := CE_BadName;
    end;
  finally
    LWord.Free; // libération du mot de travail
  end;
end;

function TGVLogoKernel.RemoveObj(const Name, Kind: string): TGVError;
// *** supprime un objet selon son type ***
begin
  Result := CE_None; // suppose qu'il n'y a pas d'erreur
  if Exists(Name) then // un objet portant ce nom existe-t-il ?
  begin
    if IsObj(Name, Kind) then // est-ce un objet du type demandé ?
    begin
      if IsProtected(Name) then // est-il protégé ?
        // [### Erreur: élément protégé ###]
        Result := CE_Protected
      else
      begin
        // sinon, on supprime la propriété
        fWorkZone.RemoveProp(Name, Kind);
        // s'il ne reste que la propriété de paquet => on supprime l'objet
        if (fWorkZone.CountProps(Name) = 1) and
          fWorkZone.IsProp(Name, CInPackage) then
            fWorkZone.RemoveListP(Name);
        Change; // changement notifié si effectif
      end;
    end
    else
      // [### Erreur : l'objet est d'une autre nature ###]
      Result := CE_BadObj;
  end
  else
    // [### Erreur: objet inconnu ###]
    Result := CE_UnKnownObject;
end;

function TGVLogoKernel.RemoveSomeObjs(const L, Kind: string): TGVError;
// *** suppression d'une liste d'objets ***
var
  LL: TGVList;
  Li: Integer;
begin
  Result := CE_None; // on suppose qu'il n'y pas d'erreur
  if L = CEmptyList then // rien à traiter ?
    Exit; // on sort
  LL := TGVList.Create; // liste des objets à supprimer
  try
    LL.Text := L; // on construit la liste des objets à supprimer
    for Li := 1 to LL.Count do // on balaie la liste
    begin
      // on  supprime l'objet en cours
      Result := RemoveObj(LL[Li - 1], Kind);
      // une erreur ?
      if Result <> CE_None then
        Break; // on sort avec le numéro de l'erreur
    end;
  finally
    LL.Free; // idem
  end;
end;

function TGVLogoKernel.Save(const FileName, Lst, Kind: string): TGVError;
// *** sauvegarde générique ***
var
  LstPL: TGVPropList;
  L1, L2: TGVList;
  Li, Lj: Integer;
  LU: TGVListUtils;
begin
  Result := CE_None; // on suppose qu'il n'y a pas d'erreur
  if fTempList.IsValid(Lst) then
  begin
    LstPL := TGVPropList.Create; // liste de propriétés provisoire créée
    try
      L1 := TGVList.Create; // liste des objets à sauver
      try
        L1.Text := Lst; // on construit la liste des objets à sauver
        Li := L1.Count; // on compte les éléments
        if Li = 0 then
        begin
          // [### Erreur: liste vide ###]
          Result := CE_EmptyList;
          Exit; // on sort
        end;
        L2 := TGVList.Create;
        try
          while (Li > 0) do // on transfère uniquement les objets existants
          begin
            if ((Kind = CExtLP) and Exists(L1[Li - 1])) or
              fWorkZone.IsProp(L1[Li - 1], Kind) then // si trouvé on mémorise
            begin
              // on recherche les propriétés
              L2.Text := fWorkZone.ValListP(L1[Li - 1]);
              for Lj := 1 to L2.Count do
                // pas le drapeau paquet
                if Odd(Lj) and (L2[Lj - 1] <> CInPackage) and
                  (L2[Lj - 1] <> CPackage) then
                begin
                  LU := TGVListUtils.Create; // utilitaire créé
                  try
                    LstPL.UpDateListP(L1[Li - 1], L2[Lj - 1],
                      LU.ListToStr(L2[Lj]));
                  finally
                    LU.Free; // utilitaire libéré
                  end;
                end;
            end
            else
            begin
              // [### Erreur: mauvaise liste ###]
              Result := CE_BadList;
              Break; // on sort de la boucle
            end;
            Dec(Li); // objet suivant
         end;
        finally
          L2.Free;
        end;
        if (Result= CE_None) then // si tout va bien, on sauvegarde
          try // on sauvegarde les objets avec la bonne extension
            LstPL.SaveToFile(ChangeFileExt(FileName, Kind));
          except
            // [### Erreur : mauvaise sauvegarde ###]
            Result := CE_BadSave;
          end;
      finally
        L1.Free; // libération de la liste de travail
      end;
    finally
      LstPL.Free; // libération de la liste provisoire
    end;
  end
  else
    // [### Erreur: mauvaise liste ###]
    Result := CE_BadList2;
end;

function TGVLogoKernel.Load(const FileName, Kind: string): TGVError;
// *** chargement générique ***
var
  L: TStringList;
  Lst: TGVList;
  LStFile, Ls: string;
  Li, Lj: Integer;
  LU: TGVListUtils;
begin
  Result := CE_None; // pas d'erreur par défaut
  L := TStringList.Create; // on crée la liste provisoire
  try
    try
      // on s'assure de la bonne extension
      if PosEx(CDot, FileName) = 0 then // si aucune extension
        LStFile := FileName + Kind // on en ajoute une
      else
        LStFile := FileName;
      if FileExists(LStFile) then // s'il existe
        L.LoadFromFile(LStFile) // on charge le fichier
      else
        // [### Erreur: fichier introuvable ###]
        Result := CE_FileNotFound;
      // si OK on vérifie son contenu
      if (Result = CE_None) then
      begin
        if L.Count < 1 then // fichier vide ?
        begin
          // [### Erreur: mauvais contenu ###]
          Result := CE_BadContent;
          Exit; // on sort
        end;
        if L[0] = CHeader then // est-ce la bonne version ?
        begin
          L.Delete(0); // on élimine l'entête
          Lst := TGVList.Create;
          L.NameValueSeparator := CSep; // séparateur entre noms et valeurs
          try
            if (Result = CE_None) then // on continue si pas d'erreur
            begin
              for Li := 0 to L.Count - 1 do // on balaie le fichier
              begin
                Ls := L.Names[Li]; // on a le nom
                // est-ce que l'objet est protégé ?
                if IsProtected(Ls) then
                begin
                  // [### Erreur: l'objet est protégé ###]
                  Result := CE_Protected;
                  Exit; // on sort
                end;
                Lst.Text := L.Values[Ls]; // les propriétés
                // est-ce le type cherché ?
                if (Kind <> CExtLP) and (Lst.First <> Kind) then
                begin
                  // [### Erreur: l'objet est incorrect ###]
                  Result := CE_BadContent;
                  Exit;
                end;
                // est-ce que le nom est déjà pris par un objet
                // de nature différente ?
                if Exists(Ls) then
                  for Lj := 1 to Lst.Count do
                    if Odd(Lj) and not fWorkZone.IsProp(Ls, Lst[Lj - 1]) then
                    begin
                      // [### Erreur: mauvais objet ###]
                      Result := CE_BadObj;
                      Exit; // on sort
                    end;
                // sinon les stocke
                for Lj := 1 to Lst.Count do
                  if Odd(Lj) then // par paires
                  begin
                    LU := TGVListUtils.Create; // utilitaire
                    try
                      if not fWorkZone.UpDateListP(Ls, Lst[Lj - 1],
                        LU.ListToStr(Lst[Lj])) then
                      begin
                        // [### Erreur: le contenu est incorrect ###]
                        Result := CE_BadContent;
                        Exit;
                      end;
                    finally
                      LU.Free; // libération de l'utilitaire
                    end;
                  end;
              end;
            end;
          finally
            Lst.Free; // on libère la liste de travail
          end;
        end
        else
          // [### Erreur: erreur de version ###]
          Result := CE_Version;
      end;
    except
      // [### Erreur: mauvais fichier ###]
      Result := CE_BadFile;
    end;
  finally
    L.Free;
    Change; // toujours signaler un changement
  end;
end;

procedure TGVLogoKernel.RemoveAllObjs(const Kind: string);
// *** supression d'un type d'objet donné ***
var
  LS: string;
begin
  for LS in fWorkZone do // on balaie la liste de travail
    // est-ce un objet de même type et non enterré ? (CExtLP ne filtre rien)
    if ((Kind = CExtLP) or IsObj(LS, Kind)) and not IsProtected(LS) then
      fWorkZone.RemoveListP(LS); // on le détruit
end;

function TGVLogoKernel.ObjsToList(const Kind: string): string;
// *** liste d'objets d'un type donné ***
var
  LS: string;
begin
  Result := CBeginList; // début de liste
  try
    for LS in fWorkZone do // on balaie la zone de travail
      if (LS <> EmptyStr) // cherche la propriété non vide
         // CExtLP ne filtre rien
         and ((Kind = CExtLP) or fWorkZone.IsProp(LS, Kind)) then
           Result := Result + LS + CBlank; // si OK on ajoute le nom
  finally
    Result := TrimRight(Result) + CEndList; // on ferme la liste
  end;
end;

constructor TGVLogoKernel.Create;
// *** création ***
begin
  inherited Create; // on hérite
  fWorkZone := TGVPropList.Create; // on crée la liste de travail
  fTempList := TGVListUtils.Create; // liste temporaire de travail
  fError := TGVErrors.Create; // gestion des erreurs
  fProtected := False; // pas de protection par défaut
  OnChange := nil; // gestionnaire de changement inactif
  Clear; // on nettoie
end;

destructor TGVLogoKernel.Destroy;
// *** destructon ***
begin
  OnChange := nil; // gestionnaire de changement à nil
  fWorkZone.Free; // on libère la zone de travail
  fTempList.Free; // libération de la liste de travail
  Error.Free; // on libère le gestionnaire des erreurs
  inherited Destroy; // on hérite
end;

procedure TGVLogoKernel.Clear;
// *** remise à zéro du noyau ***
begin
  fWorkZone.Clear; // on nettoie
  Error.Clear; // pas d'erreur
  fProtected := False; // pas de protection
  fTempList.Clear; // utilitaire de liste nettoyé
end;

function TGVLogoKernel.Count: Integer;
// *** renvoie le nombre d'éléments du noyau ***
begin
  Result := fWorkZone.CountListP;
end;

procedure TGVLogoKernel.Dump(Lst: TStrings);
// *** renvoie tout l'espace de travail ***
var
  Li: Integer;
begin
  for Li := 1 to fWorkZone.CountListP do
    Lst.Append(fWorkZone[Li]);
end;

function TGVLogoKernel.IsVar(const Name: string): Boolean;
// *** variable ? ***
begin
  Result := IsObj(Name, CVr);
end;

function TGVLogoKernel.VarsCount: Integer;
// *** nombre de variables ***
begin
  Result := CountObj(CVr);
end;

function TGVLogoKernel.AddVar(const Name, Value: string): Boolean;
// *** ajout ou mise à jour d'une variable ***
var
  LErr: TGVError;
begin
  LErr := AddObj(Name, CVr, Value); // on tente d'ajouter
  Result := (LErr = CE_None); // OK ?
  if not Result then
    // [### Erreur: celle renvoyée par AddObj ###]
    Error.SetError(LErr, Name);
end;

function TGVLogoKernel.ValVar(const Name: string; out Value: string): Boolean;
// *** valeur d'une variable ***
begin
  Result := False; // suppose une erreur
  if IsVar(Name) then // est-ce une variable ?
  begin
    // on rend sa valeur
    Value := fWorkZone.ValProp(Name, CVr);
    Result := True; // tout est OK
  end
  else
    // [### Erreur: ce n'est pas une variable ###]
    Error.SetError(CE_UnKnownVar, Name);
end;

function TGVLogoKernel.ValVar(const Name: string): string;
// *** valeur d'une variable ***
begin
  if IsVar(Name) then // est-ce une variable ?
    // on rend sa valeur
    Result := fWorkZone.ValProp(Name, CVr)
  else
    // [### Erreur: ce n'est pas une variable ###]
    Error.SetError(CE_UnKnownVar, Name);
end;

function TGVLogoKernel.RemoveVar(const Name: string): Boolean;
// *** suppression d'une variable ***
var
  LErr: TGVError;
begin
  LErr := RemoveObj(Name, CVr); // on tente de supprimer la variable
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: celle renvoyée par RemoveObj ###]
    Error.SetError(LErr, Name);
end;

function TGVLogoKernel.RemoveSomeVars(const Lst: string): Boolean;
// *** suppression d'une liste de variables ***
var
  LErr: TGVError;
begin
  LErr := RemoveSomeObjs(Lst, CVr); // on tente de supprimer les variables
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: celle renvoyée par RemoveSomeObjs ###]
    Error.SetError(LErr, Lst);
end;

function TGVLogoKernel.RemoveAllVars: Boolean;
// *** supression de toutes les variables ***
begin
  Result := RemoveSomeVars(VarsToList);
end;

function TGVLogoKernel.VarsToList: string;
// *** liste des variables ***
begin
  Result := ObjsToList(CVr);
end;

function TGVLogoKernel.LoadVars(const FileName: string): Boolean;
// *** chargement de variables ***
var
  LErr: TGVError;
begin
  Result := False; // erreur par défaut
  // on charge en écrasant si nécessaire
  LErr := Load(FileName, CVr);
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: valeur rendue par Load ###]
    Error.SetError(LErr, FileName);
end;

function TGVLogoKernel.SaveVars(const FileName, Lst: string): Boolean;
// *** sauve une liste de variables ***
var
  LErr: TGVError;
begin
  LErr := Save(FileName, Lst, CVr); // on tente la sauvegarde
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: valeur rendue par Save ###]
    case LErr of
      // [### Erreur: fichier ###]
      CE_BadSave: Error.SetError(LErr, FileName);
    else
      // [### Erreur: liste ###]
      Error.SetError(LErr, Lst);
    end;
end;

function TGVLogoKernel.SaveAllVars(const FileName: string): Boolean;
// *** sauvegarde de toutes les variables ***
begin
  Result := SaveVars(FileName, VarsToList);
end;

function TGVLogoKernel.IsProc(const Name: string): Boolean;
// *** procédure ? ***
begin
  Result := IsObj(Name, CProc);
end;

function TGVLogoKernel.ProcsCount: Integer;
// *** nombre de procédures ***
begin
  Result := CountObj(CProc);
end;

function TGVLogoKernel.ProcsToList: string;
// *** liste des procédures ***
begin
  Result := ObjsToList(CProc);
end;

function TGVLogoKernel.AddProc(const Name, Lst: string): Boolean;
// *** ajout d'une procédure ***
var
  LErr: TGVError;
begin
  Result := False; // erreur par défaut
  if IsValidDef(Lst) then // définition correcte ?
  begin
    LErr := AddObj(Name, CProc, Lst); // on tente d'ajouter
    Result := (LErr = CE_None); // OK ?
    if not Result then
      // [### Erreur: celle renvoyée par AddObj ###]
      Error.SetError(LErr, Name);
  end;
end;

function TGVLogoKernel.RemoveProc(const Name: string): Boolean;
// *** suppression d'une procédure ***
var
  LErr: TGVError;
begin
  LErr := RemoveObj(Name, CProc); // on tente de supprimer la procédure
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: celle renvoyée par RemoveObj ###]
    Error.SetError(LErr, Name);
end;

function TGVLogoKernel.RemoveSomeProcs(const Lst: string): Boolean;
// *** suppression d'une liste de procédures ***
var
  LErr: TGVError;
begin
  LErr := RemoveSomeObjs(Lst, CProc); // on tente de supprimer les procédures
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: celle renvoyée par RemoveSomeObjs ###]
    Error.SetError(LErr, Lst);
end;

function TGVLogoKernel.RemoveAllProcs: Boolean;
// *** supression de toutes les procédures ***
begin
  Result := RemoveSomeProcs(ProcsToList);
end;

function TGVLogoKernel.ParamsCount(const Name: string): Integer;
// *** nombre de paramètres d'une procédure ***
var
  Lst: TGVList;
begin
  Result := -1; // nombre si erreur
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := ParamsLine(Name); // on récupère la ligne des paramètres
      Result := Lst.Count; // nombre d'éléments
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end
  else
    // [### Erreur: ce n'est pas une procédure ###]
    Error.SetError(CE_UnKnownProc, Name);
end;

function TGVLogoKernel.ParamsLine(const Name: string): string;
// *** récupération de la ligne des paramètres d'une procédure  ***
var
  Lst: TGVList;
begin
  Result := EmptyStr; // vide par défaut
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create;
    try
      Lst.Text := RProp(Name, CProc); // recherche de la propriété
      Result := Lst.First; // recherche des paramètres
    finally
      Lst.Free; // libère la liste de travail
    end;
  end;
end;

function TGVLogoKernel.ParamsLine(const Name: string; out ParLine: string
  ): Boolean;
// *** ligne de paramètres d'une procédure ***
var
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create;
    try
      Lst.Text := RProp(Name, CProc); // création
      ParLine := Lst.First; // recherche des paramètres
      Result := True; // tout est OK
    finally
      Lst.Free; // libère la liste de travail
    end;
  end
  else
    // [### Erreur: ce n'est pas une procédure ###]
    Error.SetError(CE_UnKnownProc, Name);
end;

function TGVLogoKernel.ParamNum(const Name: string; Num: Integer): string;
// *** renvoie un paramètre d'une procédure ***
var
  Lst: TGVList;
begin
  Result := EmptyStr;
  if IsProc(Name) then // est-ce une procédure ?
  begin
    if (Num <= ParamsCount(Name)) and (Num > 0) then
    begin
      Lst := TGVList.Create; // liste de travail
      try
        Lst.Text := ParamsLine(Name); // ligne des paramètres
        Result := Lst[Num - 1]; // paramètre si possible
      finally
        Lst.Free; // libération de la liste de travail
      end;
    end;
  end;
end;

function TGVLogoKernel.ParamNum(const Name: string; Num: Integer; out
  ParNum: string): Boolean;
// *** renvoie un paramètre d'une procédure ***
var
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  if IsProc(Name) then // est-ce une procédure ?
  begin
    if (Num <= ParamsCount(Name)) and (Num > 0) then
    begin
      Lst := TGVList.Create; // liste de travail
      try
        Lst.Text := ParamsLine(Name); // ligne des paramètres
        ParNum := Lst[Num - 1]; // paramètre si possible
        Result := True;
      finally
        Lst.Free; // libération de la liste de travail
      end;
    end
    else
      // [### Erreur: mauvais paramètre ###]
      Error.SetError(CE_BadParam, IntToStr(Num));
  end
  else
    // [### Erreur: ce n'est pas une procédure ###]
    Error.SetError(CE_UnKnownProc, Name);
end;

function TGVLogoKernel.ProcLinesCount(const Name: string): Integer;
// *** nombre de lignes d'une définition ***
var
  Lst: TGVList;
begin
  Result := -1; // mauvais nombre
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := ProcListDef(Name); // définition
      Result := Lst.Count; // nombre d'éléments
    finally
      Lst.Free; // libération de la liste de travail
    end;;
  end;
end;

function TGVLogoKernel.ProcLine(const Name: string; Line: Integer): string;
// *** une ligne particulière d'une procédure ***
var
  Lst: TGVList;
begin
  Result := EmptyStr; // chaîne vide par défaut
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := ProcListDef(Name); // recherche de la définition
      if (Line > 0) and (Line <= Lst.Count) then
        Result := Lst[Line - 1]; // paramètre si possible
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end;
end;

function TGVLogoKernel.ProcLine(const Name: string; Line: Integer; out
  PrLine: string): Boolean;
// *** une ligne particulière d'une procédure ***
var
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := ProcListDef(Name); // recherche de la définition
      if (Line > 0) and (Line <= Lst.Count) then
      begin
        PrLine := Lst[Line - 1]; // renvoie la ligne
        Result := True; // tout est OK
      end
      else
        // [### Erreur: mauvaise ligne ###]
        Error.SetError(CE_BadLine, IntToStr(Line));
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end
  else
    // [### Erreur: ce n'est pas une procédure ###]
    Error.SetError(CE_UnKnownProc, Name);
end;

function TGVLogoKernel.ProcListDef(const Name: string): string;
// *** récupération de la définition d'une procédure  ***
var
  Lst: TGVList;
begin
  Result := EmptyStr; // vide par défaut
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create;
    try
      Lst.Text := RProp(Name, CProc); // recherche de la propriété
      Result := Lst[1]; // recherche de la définition
    finally
      Lst.Free; // libère la liste de travail
    end;
  end;
end;

function TGVLogoKernel.ProcListDef(const Name: string; out PrListDef: string
  ): Boolean;
// *** liste de définition d'une procédure ***
var
  Lst: TGVList;
begin
  Result := False;
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := RProp(Name, CProc); // définition
      PrListDef := Lst[1]; // recherche de la définition
      Result := True; // tout est OK
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end
  else
    // [### Erreur: ce n'est pas une procédure ###]
    Error.SetError(CE_UnKnownProc, Name);
end;

function TGVLogoKernel.ProcToEdit(const Name: string; Lst: TStrings): Boolean;
// *** procédure vers éditeur ***
var
  Li, Lj: Integer;
  LS: string;
begin
  Result := False; // suppose une erreur
  if IsProc(Name) then // la procédure existe-t-elle ?
  begin
    LS := EmptyStr; // chaîne vide
    Lj := ParamsCount(Name); // nombre de paramètres
    if (Lj <> 0) then // si au moins un paramètre
      for Li := 1 to Lj do // les recherche
        LS := LS + CBlank + ParamNum(Name, Li); // paramètres
    Lst.Add(EmptyStr); // espace
    Lst.Add(P_To + CBlank + Name + LS); // entête
    Lj := ProcLinesCount(Name); // nombre de lignes
    if (Lj <> 0) then // s'il y a au moins une ligne
      for Li := 1 to Lj do // les recherche et les ajoute
        Lst.Add(CBlank + CBlank + fTempList.ListToStr(ProcLine(Name, Li)));
    Lst.Add(P_End); // fin de la procédure
    Result := True; // tout est OK
  end
  else
    // [### Erreur: ce n'est pas une procédure ###]
    Error.SetError(CE_UnKnownProc, Name);
end;

function TGVLogoKernel.ProcsToEdit(const LstP: string; Lst: TStrings): Boolean;
// *** envoie une liste de procédures vers un éditeur ***
var
  LL: TGVList;
  LS: string;
begin
  Result := False; // suppose une erreur
  LL := TGVList.Create;
  try
    if fTempList.IsValid(LstP) then // la liste est-elle valide ?
    begin
      LL.Text := LstP; // si oui on l'affecte à celle de travail
      for LS in LL do
      begin
        Result := ProcToEdit(LS, Lst); // on envoie une procédure
        if not Result then // on sort en cas d'erreur
        begin
          // [### Erreur: liste invalide ###]
          Error.SetError(CE_BadList, LstP);
          Break;
        end;
      end;
    end
    else
      // [### Erreur: liste invalide ###]
      Error.SetError(CE_BadList, LstP);
  finally
    LL.Free;
  end;
end;

function TGVLogoKernel.AllProcsToEdit(Lst: TStrings): Boolean;
// *** envoie toutes les procédures vers l'éditeur ***
begin
  if ProcsCount <> 0 then
    Result := ProcsToEdit(ObjsToList(CProc), Lst)
  else
    Result := True;
end;

function TGVLogoKernel.EditToProc(Editor: TStrings; FromLine, ToLine: Integer;
  out Err: Integer): Boolean;
// *** éditeur vers procédure ***
// compile le contenu d'un éditeur à partir de la ligne indiquée
// (procédure après procédure)
// Editor : liste des lignes de l'éditeur
// FromLine : première ligne à analyser
// ToLine: dernière ligne à analyser
// Err : première ligne procédure fautive ou dernière ligne analysée
var
  Line, Li: Integer;
  L1, L2: TGVList;
  LSt, LName, LDef: string;
  LDone: Boolean;
begin
  Error.OK := True; // pas d'erreur
  Err := 1; // ligne en cours pour erreur
  // on ajuste la recherche à la taille de l'éditeur
  if (ToLine > Editor.Count) or (ToLine = 0) then
    ToLine := Editor.Count;
  if FromLine < 1 then
    FromLine := 1;
  if (ToLine - FromLine) >= 0 then // s'il y a des lignes à analyser
  begin
    Line := FromLine - 1; // départ de l'analyse
    // tant qu'il y a des lignes et qu'il n'y a pas d'erreur
    while (Line < ToLine) and Error.Ok do
    begin
      L1 := TGVList.Create;
      try
        // récupère la ligne en cours
        L1.Text := CBeginList + Trim(Editor[Line]) + CEndList;
        // *** c'est une ligne vide ou un commentaire ?
        if L1.IsEmptyList or (L1.First = CComment) then
          Inc(Line) // ligne suivante
        else
        begin
          // *** est-ce Pour ?
          if AnsiSameText(L1.First, P_To) then
          begin
            try // si erreur : mauvaise définition
              // on extrait le nom de la procédure (après Pour !)
              if L1.Count > 1 then // un mot au moins suit ?
              begin
                LName := L1[1];
                LSt := CBeginList; // LSt est le caractère [
                // *** construit la liste des paramètres si nécessaire
                if L1.Count > 2 then
                  // on ajoute les paramètres (ni le Pour ni le nom)
                  for Li := 3 to L1.Count do
                    if LSt = CBeginList then
                      LSt := LSt + L1[Li - 1]
                    else
                      LSt := LSt + CBlank + L1[Li - 1];
                // on stocke les paramètres
                LDef := CBeginList + LSt + CEndList + CBlank;
                // *** on passe à la définition
                LSt := CBeginList;
                Inc(Line); // en changeant de ligne
                LDone := False; // drapeau d'opération effectuée
                repeat
                  // on examine la ligne en cours
                  L2 := TGVList.Create;
                  try
                    L2.Text := (CBeginList + Trim(Editor[Line]) + CEndList);
                    if not L2.IsEmptyList then // on ignore une ligne vide
                    begin
                      // *** c'est encore le mot POUR ? ***
                      if AnsiSameText(L2.First, P_To) then
                      begin
                        Err := Line;
                        // [### Erreur: mot POUR mal placé ###]
                        Error.SetError(CE_BadTo, LName, Err);
                      end
                      else
                      // *** c'est le mot FIN ? ***
                      if AnsiSameText(L2.First, P_End) then
                      begin
                        // on clôt la liste de définition
                        LDef := LDef + Trim(LSt) + CEndList + CEndList;
                        if AddProc(LName, LDef) then // enregistre la définition
                        begin // l'enregistrement s'est bien déroulé
                          LDone := True; // procédure enregistrée
                          Change; // on signale le changement
                        end
                        else
                          Err := Line; // ligne d'erreur
                      end
                      else
                        LSt := LSt + CBeginList + Trim(Editor[Line]) + CEndList
                          + CBlank; // on ajoute la ligne
                    end;
                  finally
                    L2.Free; // on libère la liste de travail
                  end;
                  if (Line > ToLine) and (not Error.OK) then
                  begin
                    Err := Line;
                    // [### Erreur: mot FIN non rencontré ###]
                    Error.SetError(CE_NotEnd, LName, Err);
                  end;
                  Inc(Line);
                until not Error.Ok or LDone; // on change de ligne
              end
              else
                // [### Erreur: pas de nom après POUR ###]
                Error.SetError(CE_NoName, Editor[Line], Line);
            except
              Err := Line;
              // [### Erreur: mauvaise définition ###]
              Error.SetError(CE_BadDef, LName, Err);
            end;
          end
          else
          begin
            Err := Line;
            // [### Erreur: pas de POUR ###]
            Error.SetError(CE_NotTo, Editor[FromLine], Err);
          end;
        end;
      finally
        L1.Free; // on libère la liste de travail
      end;
    end;
  end
  else
    // [### Erreur: pas de lignes à analyser ###]
    Error.SetError(CE_EmptyEdit, Editor[FromLine], Err);
  Result := Error.Ok; // OK si pas d'erreur...
end;

function TGVLogoKernel.LoadProcs(const FileName: string): Boolean;
// *** chargement de procédures ***
var
  LErr: TGVError;
begin
  Result := False; // erreur par défaut
  // on charge en écrasant si nécessaire
  LErr := Load(FileName, CProc);
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: valeur rendue par Load ###]
    Error.SetError(LErr, FileName);
end;

function TGVLogoKernel.SaveProcs(const FileName, Lst: string): Boolean;
// *** sauve une liste de procédures ***
var
  LErr: TGVError;
begin
  LErr := Save(FileName, Lst, CProc);
  Result := (LErr = CE_None); // tout est OK ?
  if not Result then
    // [### Erreur: valeur rendue par Save ###]
    case LErr of
      // [ ### Erreur: fichier ###]
      CE_BadSave: Error.SetError(LErr, FileName);
    else
      // [ ### Erreur: liste ###]
      Error.SetError(LErr, Lst);
    end;
end;

function TGVLogoKernel.SaveAllProcs(const FileName: string): Boolean;
// *** sauve toutes les procédures ***
begin
  Result := SaveProcs(FileName, ObjsToList(CProc));
end;

function TGVLogoKernel.IsValidDef(const LSt: string): Boolean;
// *** définition valide ? ***
var
  Lst1, Lst2, Lst3: TGVList;
  LT: TStrings;
  LS: string;
begin
  Result := False; // on suppose une erreur
  Lst1 := TGVList.Create;
  try
    if fTempList.IsValid(LSt) then // liste correcte ?
      Lst1.Text := LSt
    else
    begin
      // [### Erreur: mauvaise liste ###]
      Error.SetError(CE_BadList2, LSt);
      Exit; // on sort de la procédure
    end;
    Lst2 := TGVList.Create; // premier élément = paramètres
    try
      LT := TStringList.Create; // liste des paramètres pour doublons
      try
        if Lst1.First <> CEmptyList then
        begin
          Lst2.Text := Lst1.First;
          for LS in Lst2 do // on repère les paramètres
            // s'il existe déjà ou s'il est incorrect
            if (LT.IndexOf(LS) <> -1) then
            begin
              Result := False;
              // [### Erreur: paramètre dupliqué ###]
              Error.SetError(CE_DupParam, LS);
              Break; // on sort de la boucle
            end
            else
            if IsValidParam(LS) then // paramètre valide ?
            begin
              LT.Add(LS); // stocke le nouveau paramètre
              Result := True; // c'est bon...
            end
            else
            begin
              Result := False;
              // [### Erreur: mauvais paramètre ###]
              Error.SetError(CE_BadParam, LS);
              Break; // on sort de la boucle
            end;
        end
        else
          Result := True; // liste vide acceptée
      finally
        LT.Free; // on libère la liste de travail
      end;
    finally
      Lst2.Free; // idem
    end;
    if Result then // si tout va bien : on examine la définition
    begin
      Lst2 := TGVList.Create; // on analyse les lignes
      try
        if (Lst1.Count > 1) then // on doit avoir une définition
        begin
          Lst3 := TGVList.Create; // on crée la liste des lignes
          try
            Lst3.Text := Lst1[1]; // on lui affecte la liste de définition
            for LS in Lst3 do // on balaie les lignes
            begin
              // liste valide
              Result := fTempList.IsValid(LS);
              if not Result then // sinon on sort
              begin
                // [### Erreur: mauvaise définition ###]
                Error.SetError(CE_BadDef, Lst1[0]);
                Break;
              end;
            end;
          finally
            Lst3.Free; // on libère la liste
          end;
        end
        else
          // [### Erreur: mauvaise définition ###]
          Error.SetError(CE_BadDef, LSt);
      finally
        Lst2.Free; // on libère la liste
      end;
    end;
  finally
    Lst1.Free; // on libère la liste de travail
  end;
end;

function TGVLogoKernel.CopyDef(const FromProc, ToProc: string): Boolean;
// *** copie d'une procédure dans un objet ***
begin
  Result := False; // erreur par défaut
  if IsProc(FromProc) then // la procédure existe-t-elle ?
    // on essaye de créer l'autre
    Result := AddProc(ToProc, CBeginList + ParamsLine(FromProc) +
      ProcListDef(FromProc) + CEndList)
  else
    // [### Erreur: la procédure n'existe pas ###]
    Error.SetError(CE_UnknownProc, FromProc);
end;

function TGVLogoKernel.IsValidParam(const Name: string): Boolean;
// *** le paramètre est-il valide ? ***
var
  LWord: TGVWord;
begin
  Result := (Name = fTempList.EmptyList); // on accepte la liste vide
  if not Result then
  begin
    LWord := TGVWord.Create; // mot de travail créé
    try
      LWord.Text := Name; // on teste l'identificateur
      if LWord.Text <> LWord.WithoutColon then // : présents ?
      begin
        LWord.Text := LWord.WithoutColon; // on les enlève
        Result := LWord.IsValidIdent; // reste valide ?
      end;
    finally
      LWord.Free; // libération du mot de travail
    end;
  end;
end;

function TGVLogoKernel.IsPrim(const Name: string): Boolean;
// *** primitive ? ***
begin
  Result := (NumPrim(Name) <> -1);
end;

function TGVLogoKernel.PrimsCount: Integer;
// *** décompte du nombre de primitives ***
begin
  Result := CPrimCount;
end;

function TGVLogoKernel.NumPrim(const Name: string): Integer;
// *** numéro de primitive ***
var
  Li: Integer;
begin
  Result := -1; // mauvais nombre
  for Li := 1 to CPrimCount do // on balaie le tableau
  begin
    if AnsiSameText(GVPrimName[Li].Name, Name) then // trouvée ?
    begin
      Result := Li; // on stocke le résultat
      Break; // on sort de la boucle
    end;
  end;
end;

function TGVLogoKernel.NumParamsPrim(const Name: string): Integer;
// *** nombre de paramètres de la primitive ***
begin
  Result := NumPrim(Name); // on cherche la primitive
  if (Result <> -1) then // trouvée ?
    Result := GVPrimName[Result].NbParams; // on renvoie le nombre de paramètres
end;

function TGVLogoKernel.PrimsToList: string;
// *** liste des primitives ***
var
  LRec: TGVPrimRec;
begin
  Result := CBeginList; // début de liste
  try
    for LRec in GVPrimName do // on balaie le tableau
      Result := Result + LRec.Name + ' '; // on ajoute le nom trouvé
  finally
    Result := TrimRight(Result) + CEndList; // fin de liste
  end;
end;

function TGVLogoKernel.PrimByNum(const N: Integer): string;
// *** primitive par son numéro ***
begin
  if (N > 0) and (N <= CPrimCount) then // dans les bornes ?
    Result := GVPrimName[N].Name // recherche du nom de la primitive
  else
    // [### Erreur: primitive inconnue ###]
    Error.SetError(CE_UnKnownPrim, IntToStr(N)); // non !
end;

function TGVLogoKernel.PrimByNum(const N: Integer; out PrimBNum: string
  ): Boolean;
// *** primitive par son numéro ***
begin
  Result := False; // suppose une erreur
  if (N > 0) and (N <= CPrimCount) then // dans les bornes ?
  begin
    PrimBNum := GVPrimName[N].Name; // recherche du nom de la primitive
    Result := True;
  end
  else
    // [### Erreur: primitive inconnue ###]
    Error.SetError(CE_UnKnownPrim, IntToStr(N)) // non !
end;

function TGVLogoKernel.IsPck(const Name: string): Boolean;
// *** est-ce un paquet ? ***
begin
  Result := IsObj(Name, CPackage);
end;

function TGVLogoKernel.PcksCount: Integer;
// *** nombre de paquets ***
begin
  Result := CountObj(CPackage);
end;

function TGVLogoKernel.IsBurriedPck(const Name: string): Boolean;
// *** paquet enfoui ? ***
begin
  Result := False; // on suppose que non
  if IsPck(Name) then // est-ce un paquet ?
    Result := fWorkZone.IsProp(Name, CBurried); // propriété enterrée ?
end;

function TGVLogoKernel.BurryPck(const Name: string): Boolean;
// *** on enterre un paquet ***
begin
  Result := False; // on suppose une erreur
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if not IsBurriedPck(Name) then // non déjà enterré ?
    begin
      Result := fWorkZone.UpDateListP(Name, CBurried, MF_True); // on l'enterre
      Change; // on notifie le changement
    end;
  end
  else
    // [### Erreur: paquet inconnu ###]
    Error.SetError(CE_UnKnownPackage, Name);
end;

function TGVLogoKernel.UnBurryPck(const Name: string): Boolean;
// *** déterre un paquet ***
begin
  Result := False; // on suppose une erreur
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if IsBurriedPck(Name) then // enterré ?
    begin
      Result := fWorkZone.RemoveProp(Name, CBurried); // on le déterre
      Change; // changement notifié
    end;
  end
  else
    // [### Erreur: pas un paquet ###]
    Error.SetError(CE_UnKnownPackage, Name);
end;

function TGVLogoKernel.ToPck(const Name, Obj: string): Boolean;
// *** ajoute un objet à un paquet ***
begin
  Result := False; // on suppose une erreur
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if Exists(Obj) then // l'objet existe-t-il ?
    begin
      if IsPck(Obj) then // est-ce un paquet ?
        // [### Erreur: ne peut être empaqueté ###]
        Error.SetError(CE_PackageForbidden, Name)
      else
      begin
        if IsProtected(Obj) then // objet protégé ?
          // [### Erreur: objet protégé ###]
          Error.SetError(CE_Protected, Obj)
        else
        begin
          // mise à jour
          Result := fWorkZone.UpDateListP(Obj, CInPackage, Name);
          Change; // changement signalé
        end;
      end;
    end
    else
      // [### Erreur: objet inconnu ###]
      Error.SetError(CE_UnKnownObject, Obj);
  end
  else
    // [### Erreur: ce n'est pas un paquet ###]
    Error.SetError(CE_UnKnownPackage, Name);
end;

function TGVLogoKernel.ListToPck(const Name, Lst: string): Boolean;
// *** liste vers paquet ***
var
  LL: TGVList;
  Li: Integer;
begin
  Result := False; // suppose une erreur
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if Lst = CEmptyList then // liste vide ?
    begin
      Result := True; // rien à faire !
      Exit;
    end;
    if fTempList.IsValid(LSt) then // liste valide ?
    begin
      LL := TGVList.Create; // liste de travail
      try
        LL.Text := Lst;
        Li := LL.Count; // nombre d'éléments
        while (Li > 0) do
        begin
          // si on parvient à empaqueter l'objet
          if ToPck(Name, LL[Li - 1]) then
            Dec(Li) // on passe au suivant
          else
            Break; // sinon c'est le dernier
        end;
        Result := (Li = 0); // pas d'erreur ?
      finally
        LL.Free; // libération de la liste de travail
      end;
    end
    else
      // [### Erreur: mauvaise liste ###]
      Error.SetError(CE_BadList2, Lst);
  end
  else
    // [### Erreur: ce n'est pas un paquet ###]
    Error.SetError(CE_UnKnownPackage, Name);
end;

function TGVLogoKernel.PckToList(const Name: string): string;
// *** liste du paquet ***
var
  LS: string;
begin
  Result := CBeginList; // on ouvre la liste
  try
    if IsPck(Name) then // est-ce un paquet ?
    begin
      for LS in fWorkZone do // on balaie la zone de travail
        if (LS <> EmptyStr) and // liste récupérée
          // si c'est la propriété paquet et si elle correspond à Name
          IsInPck(LS) and AnsiSameText(fWorkZone.ValProp(LS, CInPackage),
            Name) then
              Result := Result + LS + CBlank; // on ajoute le nom
    end
    else
      // [### Erreur: ce n'est pas un paquet ###]
      Error.SetError(CE_UnKnownPackage, Name);
    Result := TrimRight(Result); // élimine le dernier blanc
  finally
    Result := Result + CEndList; // on ferme la liste
  end;
end;

function TGVLogoKernel.CountItemsPck(const Name: string): Integer;
// *** nombre d'éléments d'un paquet ***
var
  LS: string;
begin
  Result := -1; // suppose une erreur
  if IsPck(Name) then // est-ce un paquet ?
  begin
    Result := 0; // pas d'éléments
    for LS in fWorkZone do // on balaie la zone de travail
      if (LS <> EmptyStr) and IsInPck(LS) // cherche propriété de paquet
        // la bonne ?
        and AnsiSameText(fWorkZone.ValProp(LS, CInPackage), Name) then
          Inc(Result); // si c'est bon, on incrémente le résultat
  end
  else
    // [### Erreur: ce n'est pas un paquet ###]
    Error.SetError(CE_UnKnownPackage, Name);
end;

function TGVLogoKernel.PcksToList: string;
// *** liste des paquets ***
begin
  Result := ObjsToList(CPackage);
end;

function TGVLogoKernel.CreatePck(const Name: string): Boolean;
// *** création d'un paquet ***
var
  LErr: TGVError;
begin
  Result := False; // erreur supposée
  // on tente de créer le paquet
  LErr := AddObj(Name, CPackage, MF_True);
  case LErr of
    // [### Erreur: déjà un paquet ###]
    CE_Protected: Error.SetError(CE_AlreadyPackage, Name);
    CE_None: Result := True; // tout est OK
    else
      // [### Erreur: celle de AddObj ###]
      Error.SetError(LErr, Name);
  end;
end;

function TGVLogoKernel.RemovePck(const Name: string): Boolean;
// *** destruction d'un paquet ***
var
  LS: string;
begin
  Result := False; // on suppose une erreur
  if IsPck(Name) then // est-ce un paquet ?
  begin
    for LS in fWorkZone do // on balaie la zone de travail
    begin
      // cherche la propriété paquet
      if (LS <> EmptyStr) and
        AnsiSameText(fWorkZone.ValProp(LS, CInPackage), Name) then
          // on supprime cette propriété
          fWorkZone.RemoveProp(LS, CInPackage);
    end;
    Result := fWorkZone.RemoveListP(Name); // on supprime le paquet
    Change; // changement notifié
  end
  else
    // [### Erreur: ce n'est pas un paquet ###]
    Error.SetError(CE_UnKnownPackage, Name);
end;

function TGVLogoKernel.SavePck(const Name: string): Boolean;
// *** sauvegarde d'un paquet ***
var
  LS: string;
  LErr: TGVError;
begin
  Result := False; // suppose une erreur
  if IsPck(Name) then // est-ce un paquet ?
  begin
    LS := PckToList(Name); // liste des éléments du paquet
    // sauvegarde (le nom du paquet est celui du fichier)
    LErr := Save(Name, LS, CInPackage);
    Result := (LErr = CE_None); // tout est OK ?
    if not Result then
      // [### Erreur: erreur renvoyée par Save ###]
      Error.SetError(LErr, Name);
  end
  else
    // [### Erreur : le paquet n'existe pas ###]
    Error.SetError(CE_UnKnownPackage, Name);
end;

function TGVLogoKernel.PckToEdit(const Name: string; Lst: TStrings): Boolean;
// *** paquet vers l'éditeur ***
var
  LS: string;
begin
  Result := False; // suppose une erreur
  if IsPck(Name) then // est-ce un paquet ?
  begin
    for LS in fWorkZone do // on balaie la zone de travail
    begin
      if (LS <> EmptyStr) and
      // on ajoute si procédure et propriété présentes et correctes
        IsProc(LS) and IsInPck(LS) and
          AnsiSameText(fWorkZone.ValProp(LS, CInPackage), Name) then
      begin
        Result := ProcToEdit(LS, Lst); // on ajoute la procédure
        if not Result then
          Break; // si erreur on sort de la boucle
      end;
      Application.ProcessMessages; // on traite les messages
    end;
  end
  else
    // [### Erreur: pas un paquet ###]
    Error.SetError(CE_UnKnownPackage, Name);
end;

function TGVLogoKernel.UnPackObj(const Name: string): Boolean;
// *** dépaquette un objet ***
begin
  Result := False; // suppose une erreur
  if Exists(Name) then // l'objet existe-t-il ?
  begin
    if IsInPck(Name) then // est-il dans un paquet ?
    begin
      // paquet enfoui ?
      if IsBurriedPck(fWorkZone.ValProp(Name, CInPackage)) then
        // [### Erreur: paquet enfoui ###]
        Error.SetError(CE_Burried, Name)
      else
        // on enlève la propriété de paquet
        Result := fWorkZone.RemoveProp(Name, CInPackage);
    end;
  end
  else
    // [### Erreur: objet inconnu ###]
    Error.SetError(CE_UnKnownObject, Name);
end;

function TGVLogoKernel.IsInPck(const Name: string): Boolean;
// *** l'objet est-il dans un paquet ? ***
begin
  Result := IsObj(Name, CInPackage);
end;

function TGVLogoKernel.IsBurried(const Name: string): Boolean;
// *** l'objet est-il enterré ? ***
begin
  Result := False; // suppose une erreur
  if IsPck(Name) then // un paquet ?
    Result := IsBurriedPck(Name) // enfoui ?
  else
  if IsInPck(Name) then // si dans un paquet
    // cherche si le paquet est enfoui
    Result := IsBurriedPck(BelongsTo(Name));
end;

function TGVLogoKernel.BelongsTo(const Name: string): string;
// *** à quel paquet appartient l'objet ? ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if Exists(Name) then // l'objet existe-t-il ?
  begin
    if IsInPck(Name) then // est-il dans un paquet ?
      Result := fWorkZone.ValProp(Name, CInPackage) // si oui on cherche lequel
    else
      // [### Erreur: pas dans un paquet ###]
      Error.SetError(CE_NotInPackage, Name);
  end
  else
    // [### Erreur: objet inconnu ###]
    Error.SetError(CE_UnKnownObject, Name);
end;

function TGVLogoKernel.BelongsTo(const Name: string; out Which: string
  ): Boolean;
// *** à quel paquet appartient l'objet ? (sans erreur) ***
begin
  Result := False; // erreur par défaut
  if Exists(Name) then // l'objet existe-t-il ?
  begin
    if IsInPck(Name) then // est-il dans un paquet ?
    begin
      // si oui on cherche lequel
      Which := fWorkZone.ValProp(Name, CInPackage);
      Result := True; // paquet trouvé
    end;
  end;
end;

function TGVLogoKernel.DProp(const Name, Prop, Value: string): Boolean;
// *** définition d'une propriété (même protégée ) ***
begin
  Result := False; // suppose une erreur
  if IsPrim(Name) then // est-ce une primitive ?
    // [### Erreur: primitive non modifiable ###]
    Error.SetError(CE_CantModifyPrim, Name)
  else
  begin
    Result := fWorkZone.UpDateListP(Name, Prop, Value); // mise à jour
    if not Result then
      // [### Erreur: mauvaise propriété ###]
      Error.SetError(CE_BadProp, Name);
  end;
end;

function TGVLogoKernel.RProp(const Name, Prop: string): string;
// *** valeur d'une propriété ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if Exists(Name) then // nom existe ?
  begin
    if fWorkZone.IsProp(Name, Prop) then
      Result := fWorkZone.ValProp(Name, Prop) // renvoie la valeur
    else
      // [### Erreur: propriété inconnue ###]
      Error.SetError(CE_UnKnownProp, Prop);
  end
  else
    // [### Erreur: liste de propriétés inconnue ###]
    Error.SetError(CE_UnKnownListP, Name);
end;

function TGVLogoKernel.RProp(const Name, Prop: string; out Value: string
  ): Boolean;
// *** valeur d'une propriété ***
begin
  Result := False; // suppose une erreur
  if Exists(Name) then // nom existe ?
  begin
    if fWorkZone.IsProp(Name, Prop) then
    begin
      Value := fWorkZone.ValProp(Name, Prop); // renvoie la valeur
      Result := True;
    end
    else
      // [### Erreur: propriété inconnue ###]
      Error.SetError(CE_UnKnownProp, Prop);
  end
  else
    // [### Erreur: liste de propriétés inconnue ###]
    Error.SetError(CE_UnKnownListP, Name);
end;

function TGVLogoKernel.AnProp(const Name, Prop: string): Boolean;
// *** annulation d'une propriété ***
begin
  Result := False; // suppose une erreur
  if Exists(Name) then // est-ce une liste ?
  begin
    if IsPrim(Name) then // est-ce une primitive ?
      // [### Erreur: primitive non modifiable ###]
      Error.SetError(CE_CantModifyPrim, Name)
    else
    begin
      Result := fWorkZone.RemoveProp(Name, Prop); // détruire la propriété liée
      if Result then
      begin
        if IsEmptyPList(Name) then // la liste finale est-elle vide ?
          Result := fWorkZone.RemoveListP(Name); // on supprime la liste
      end
      else
        // [### Erreur: propriété inconnue ###]
        Error.SetError(CE_UnknownProp, Prop);
      if Result then
        Change; // on notifie le changement si effectif
    end;
  end
  else
    // [### Erreur: liste de propriétés inconnue ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVLogoKernel.PListe(const Name: string): string;
// *** liste associée à une propriété ***
begin
  Result := fWorkZone.ValListP(Name); // renvoie la liste
  if Result = EmptyStr then // ne peut être une chaîne vide
    // [### Erreur: chaîne vide ###]
    Error.SetError(CE_UnKnownListP, Name); // pas une propriété
end;

function TGVLogoKernel.PListe(const Name: string; out Value: string): Boolean;
// *** liste associée à une propriété ***
begin
  Result := False; // suppose une erreur
  Value := fWorkZone.ValListP(Name); // renvoie la liste
  if Value = EmptyStr then // ne peut être une chaîne vide
    // [### Erreur: chaîne vide ###]
    Error.SetError(CE_UnKnownListP, Name) // pas une propriété
  else
    Result := True; // tout est OK
end;

function TGVLogoKernel.GList(const Prop: string): string;
// *** objets ayant une propriété donnée ***
begin
  Result := ObjsToList(Prop);
end;

function TGVLogoKernel.IsEmptyPList(const Name: string): Boolean;
// *** la liste de propriété est-elle vide ? ***
begin
  Result := (fWorkZone.ValListP(Name) = EmptyStr);
end;

function TGVLogoKernel.IsProp(const Name, Prop: string): Boolean;
// *** la propriété est-elle dans la liste de propriétés ?
begin
  Result := fWorkZone.IsProp(Name, Prop);
end;

function TGVLogoKernel.IsListP(const Name: string): Boolean;
// *** la liste de propriétés existe-t-elle ? ***
begin
  Result := fWorkZone.IsListP(Name);
end;

end.

