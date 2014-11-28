{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Noyau de GVLOGO                         |
  |                  Unité : GVKernel.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    27-11-2014 13:17:40                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

{$I GVDefines.inc}

unit GVKernel;

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
// Unité du noyau de GVLOGO : gestion de l'espace mémoire
//
// ##############################################################
//
// Le noyau permet de gérer l'espace mémoire dédié à GVLOGO. En particulier,
// sont traités les variables, les procédures, les primitives, les paquets, les
// listes de propriétés, les propriétés et divers objets.
//
// Le noyau est un des éléments fondamentaux
// de l'interpréteur de GVLOGO.
//

interface

uses Classes,
  GVConsts,
  GVPropLists,
  GVLists;

type
  // *** classe pour le noyau

  { événement lors d'une erreur }
  TGVKernelEvent = procedure(const Err: TGVError; Message: string) of object;

  { TGVLogoKernel }

  TGVLogoKernel = class(TObject)
  private
    fProtected: Boolean; // drapeau de protection
    fWorkZone: TGVPropList; // zone de travail
    fLocalVars: TGVPropList; // zone des variables locales
    fKernelResult: TGVError; // numéro d'erreur
    fOnChange: TNotifyEvent; // notification des changements
    fOnKernelError: TGVKernelEvent; // notification d'une erreur
    fTempList: TGVListUtils; // liste de travail
    function GetKernelResult: TGVError; // résultat d'une opération
    function GetError: Boolean; // erreur ?
    procedure SetProtected(AValue: Boolean); // protection des données
  protected
    // gestion des changements
    procedure Change; dynamic;
    // remet à zéro les erreurs
    procedure ClearError; inline;
    // sauvegarde générique
    function Save(const FileName, Lst, Kind: string): Boolean;
    // chargement générique
    function Load(const FileName, Kind: string): Boolean;
    // compte d'objets d'un type donné
    function ObjCount(const Kind: string): Integer;
    // est-ce un type d'objet donné ?
    function IsObj(const Name, Kind: string): Boolean;
    // supression d'un type d'objet donné
    function RemoveAllObj(const Kind: string): Boolean;
    // supression d'un objet
    function RemoveObj(const Name, Kind: string): Boolean;
    // suppression d'une liste d'objets
    function RemoveSomeObj(const Lst, Kind: string): Boolean;
  public
    // *** gestion ***

    // création
    constructor Create;
    // destruction
    destructor Destroy; override;
    // remise à zéro de l'espace de travail
    procedure Clear;
    // message d'erreur
    procedure ErrorMessage(const Err: TGVError; Title: string);

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
    // nom de variable valide ?
    function IsValidVar(const Name: string): Boolean;

    // *** traitement des variables locales ***

    // est-ce une variable locale ?
    function IsLocVar(const Name: string): Boolean;
    // nombre de variables locales
    function LocVarsCount: Integer;
    // affectation à une variable locale
    function AddLocVar(const Name, Value: string): Boolean;
    // valeur d'une variable locale
    function ValLocVar(const Name: string; out Value: string): Boolean;
      overload;
    function ValLocVar(const Name: string): string; overload;
    // destruction d'une variable locale
    function RemoveLocVar(const Name: string): Boolean;
    // destruction de toutes les variables locales
    procedure RemoveAllLocVars;
    // liste des variables locales
    function LocVarsToList: string;

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
      out Error: Integer): Boolean;
    // charge des procédures
    function LoadProcs(const FileName: string): Boolean;
    // sauve des procédures
    function SaveProcs(const FileName, Lst: string): Boolean;
    // sauvegarde toutes les procédures
    function SaveAllProcs(const FileName: string): Boolean;
    // définition valide ?
    function IsValidDef(const St: string): Boolean;
    // paramètre valide ?
    function IsValidParam(const Lst: string): Boolean;

    // *** traitement des primitives ***

    // l'objet est-il une primitive ?
    function IsPrim(const Name: string): Boolean;
    // nombre de primitives
    function PrimsCount: Integer;
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
    // empaquette tout
    function PckAll(const Name: string): Boolean;
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

    // *** objets généraux ***

    // rend tout l'espace de travail
    procedure Dump(Lst: TStrings);
    // l'objet existe-t-il ?
    function Exists(const Name: string): Boolean;
    // erreur si l'objet n'existe pas
    function TestExistsObj(const Name: string): Boolean;
    // l'objet est-il protégé ?
    function IsProtected(const Name: string): Boolean;
    // renvoie le nombre d'objets
    function Count: Integer;
    // renvoie les objets de l'espace de travail
    function ToList: string;
    // le nom est-il valide ?
    function IsValid(const Name: string): Boolean;
    // charge tout l'espace de travail
    function LoadAll(const FileName: string): Boolean;
    // sauvegarde tout l'espace de travail
    function SaveAll(const FileName: string): Boolean;

    // *** propriétés ***

    // erreur ?
    property Error: Boolean read GetError;
    // numéro de l'erreur ?
    property KernelResult: TGVError read GetKernelResult default C_None;
    // protection ?
    property Protected: Boolean read fProtected write SetProtected default False;
    // événement si changement
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    // événement si erreur
    property OnKernelError: TGVKernelEvent read fOnKernelError
      write fOnKernelError;
  end;

implementation

uses SysUtils,
  Forms,
  StrUtils;

{ TGVLogoKernel }

function TGVLogoKernel.AddLocVar(const Name, Value: string): Boolean;
// *** ajout d'une variable locale ***
begin
  ClearError; // pas de message
  Result := False; // on suppose une erreur
  if IsValid(Name) then // nom corect ?
  begin
    // on essaie de créer ou de mettre à jour la variable locale
    Result := fLocalVars.UpDateListP(Name, CVr, CBeginList + Value + CEndList);
    if Result then
      Change; // notifie le changement si effectif
  end
  else
    ErrorMessage(C_BadName, Name); // nom incorrect
end;

(* ********************************************************************* *)

function TGVLogoKernel.AddProc(const Name, Lst: string): Boolean;
// *** ajout d'une procédure ***

  procedure StoreProc;
  // enregistre une procédure
  begin
    Result := DProp(Name, CProc, Lst); // définition
    if Result then
      Change; // notifie le changement si ajout effectif
  end;

begin
  Result := False; // on suppose une erreur
  ClearError; // rien à signaler
  if Exists(Name) then // si le nom existe
  begin
    if IsProc(Name) then // si oui, est-ce une procédure ?
    begin
      if IsProtected(Name) then // est-elle protégée ?
        ErrorMessage(C_Protected, Name) // erreur
      else if IsValidDef(Lst) then // mise à jour si correcte
        StoreProc; // enregistre la procédure
    end
    else // ce n'est pas une procédure
    begin
      if IsProtected(Name) then // l'objet est-il protégé ?
        ErrorMessage(C_Protected, Name) // erreur
      else
      begin
        if IsValidDef(Lst) and // définition valide ?
          fWorkZone.RemoveListP(Name) then // on le détruit
            StoreProc; // enregistre la procédure
      end;
    end;
  end
  else // le nom n'existe pas
  begin
    if IsValid(Name) then // nom correct ?
    begin
      if IsValidDef(Lst) then // définition correcte ?
        StoreProc; // enregistre la procédure
    end
    else
      ErrorMessage(C_BadName, Name); // nom incorrect
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.AddVar(const Name, Value: string): Boolean;
// *** ajout d'une variable ou mise à jour ***
begin
  Result := False; // on suppose une erreur
  ClearError; // pas de message
  if Exists(Name) then // le nom est-il pris ?
  begin
    if IsVar(Name) then // est-ce une variable ?
    begin
      if IsProtected(Name) then // protégée ?
        ErrorMessage(C_Protected, Name) // si oui, erreur
      else
      begin
        // sinon la mettre à jour
        Result := DProp(Name, CVr, CBeginList + Value + CEndList);
        if Result then
          Change; // notifie le changement si effectif
      end;
    end
    else
      ErrorMessage(C_NotVar, Name); // pas une variable : erreur
  end
  else
  begin // le nom n'est pas pris
    if IsValid(Name) then // le nom est-il correct ?
    begin
      // créer la variable
      Result := DProp(Name, CVr, CBeginList + Value + CEndList);
      if Result then
        Change; // notifie le changement si effectif
    end
    else
      ErrorMessage(C_BadName, Name); // nom incorrect
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.AllProcsToEdit(Lst: TStrings): Boolean;
// envoie toutes les procédures vers l'éditeur
begin
  Result := ProcsToEdit(GList(CProc), Lst);
end;

(* ********************************************************************* *)

function TGVLogoKernel.AnProp(const Name, Prop: string): Boolean;
// *** annulation d'une propriété ***
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if Exists(Name) then // est-ce une liste ?
  begin
    if IsPrim(Name) then // est-ce une primitive ?
      ErrorMessage(C_PrimForbidden, Name) // on l'épargne
    else
    begin
      Result := fWorkZone.RemoveProp(Name, Prop); // détruire la propriété liée
      if Result then
      begin
        if IsEmptyPList(Name) then // la liste finale est-elle vide ?
          Result := fWorkZone.RemoveListP(Name); // on supprime la liste
      end
      else
        ErrorMessage(C_NotProp, Prop); // sinon propriété inconnue
      if Result then
        Change; // on notifie le changement si effectif
    end;
  end
  else
    ErrorMessage(C_NotLProp, Name); // liste inconnue
end;

(* ********************************************************************* *)

function TGVLogoKernel.BelongsTo(const Name: string; out Which: string)
  : Boolean;
// *** à quel paquet appartient l'objet ? ***
begin
  Result := False; // erreur par défaut
  ClearError; // pas de message d'erreur
  if Exists(Name) then // l'objet existe-t-il ?
  begin
    if IsInPck(Name) then // est-il dans un paquet ?
      Result := fWorkZone.ValProp(Name, CInPackage, Which)
      // si oui on cherche lequel
    else
      ErrorMessage(C_NotInPackage, Name); // pas dans un paquet
  end
  else
    ErrorMessage(C_NotObject, Name); // objet inconnubegin
end;

function TGVLogoKernel.BurryPck(const Name: string): Boolean;
// *** on enterre un paquet ***
begin
  Result := False; // on suppose une erreur
  ClearError; // pas de message
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if IsBurriedPck(Name) then // déjà enterré ?
      ErrorMessage(C_AlreadyBurried, Name) // erreur := déjà enterré
    else
    begin
      Result := DProp(Name, CBurried, MF_True); // on l'enterre
      if Result then
        Change; // on notifie le changement si effectif
    end;
  end
  else
    ErrorMessage(C_NotPackage, Name); // erreur : paquet
end;

(* ********************************************************************* *)

procedure TGVLogoKernel.Change;
// *** changement dans le noyau ***
begin
  if Assigned(fOnChange) then // si le gestionnaire existe
    fOnChange(Self); // on l'exécute
end;

(* ********************************************************************* *)

procedure TGVLogoKernel.ErrorMessage(const Err: TGVError; Title: string);
// *** message si erreur ***
begin
  fKernelResult := Err; // on mémorise l'erreur
  if Assigned(fOnKernelError) then // si le gestionnaire existe
    fOnKernelError(Err, Title); // on l'exécute
end;

(* ********************************************************************* *)

procedure TGVLogoKernel.ClearError;
// *** remet à zéro les erreurs ***
begin
  fKernelResult := C_None;
  ErrorMessage(C_None, EmptyStr);
end;

(* ********************************************************************* *)

procedure TGVLogoKernel.Clear;
// *** remise à zéro du noyau ***
begin
  ClearError; // pas d'erreur
  fWorkZone.Clear; // on nettoie
end;

(* ********************************************************************* *)

function TGVLogoKernel.Count: Integer;
// *** renvoie le nombre d'éléments du noyau ***
begin
  ClearError; // rien à signaler
  Result := fWorkZone.CountListP;
end;

(* ********************************************************************* *)

function TGVLogoKernel.CountItemsPck(const Name: string): Integer;
// *** nombre d'éléments d'un paquet ***
var
  S: string;
begin
  Result := -1; // suppose une erreur
  ClearError; // pas de message
  if IsPck(Name) then // est-ce un paquet ?
  begin
    Result := 0; // pas d'éléments
    for S in fWorkZone do // on balaie la zone de travail
      if (S <> EmptyStr) and IsInPck(S) // cherche propriété de paquet
        and AnsiSameText(BelongsTo(S), Name) then // la bonne ?
        Inc(Result); // si c'est bon, on incrémente le résultat
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

constructor TGVLogoKernel.Create;
// *** création ***
begin
  inherited Create; // on hérite
  fWorkZone := TGVPropList.Create; // on crée la liste de travail
  fLocalVars := TGVPropList.Create; // on crée la zone des variables locales
  fTempList := TGVListUtils.Create; // liste temporaire de travail
  fProtected := False; // pas de protection par défaut
  OnChange := nil; // gestionnaires à nil
  OnKernelError := nil;
  Clear; // on nettoie
end;

(* ********************************************************************* *)

function TGVLogoKernel.CreatePck(const Name: string): Boolean;
// *** création d'un paquet ***
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if Exists(Name) then // le nom est-il pris ?
  begin
    if IsPck(Name) then // si oui, est-ce un paquet ?
      ErrorMessage(C_AlreadyPackage, Name) // si oui, erreur
    else
    begin
      if IsProtected(Name) then // élément protégé ?
        ErrorMessage(C_Protected, Name) // erreur
      else
      begin
        if fWorkZone.RemoveListP(Name) then // on le détruit
          Result := DProp(Name, CPackage, MF_True); // on le remplace
        if Result then
          Change; // changement notifié si effectif
      end;
    end;
  end
  else // le nom est libre
  begin
    if IsValid(Name) then // nom correct ?
    begin
      Result := DProp(Name, CPackage, MF_True); // création du paquet
      if Result then
        Change; // changement notifié si effectif
    end
    else
      ErrorMessage(C_BadName, Name); // nom incorrect
  end;
end;

(* ********************************************************************* *)

destructor TGVLogoKernel.Destroy;
// *** destruction ***
begin
  OnChange := nil; // gestionnaires à nil
  OnKernelError := nil;
  fWorkZone.Free; // on libère la zone de travail
  fLocalVars.Free; // idem pour les variables locales
  fTempList.Free; // libération de la liste de travail
  inherited Destroy; // on hérite
end;

(* ********************************************************************* *)

function TGVLogoKernel.DProp(const Name, Prop, Value: string): Boolean;
// *** définition d'une propriété ***
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if IsPrim(Name) then // est-ce une primitive ?
    ErrorMessage(C_PrimForbidden, Name) // on l'épargne
  else
  begin
    Result := fWorkZone.UpDateListP(Name, Prop, Value); // mise à jour
    if not Result then
      ErrorMessage(C_BadProp, Name); // en cas d'erreur
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.EditToProc(Editor: TStrings; FromLine, ToLine: Integer;
  out Error: Integer): Boolean;
// *** éditeur vers procédure ***
// compile le contenu d'un éditeur à partir de la ligne indiquée (procédure après procédure)
// Editor : liste des lignes de l'éditeur
// FromLine : première ligne à analyser
// ToLine: dernière ligne à analyser
// Error : première ligne procédure fautive ou dernière ligne analysée
var
  Line, I: Integer;
  L1, L2: TGVList;
  St, Name, Def: string;
begin
  ClearError; // pas de message
  Error := 1; // ligne en cours pour erreur
  // on ajuste la recherche à la taille de l'éditeur
  if (ToLine > Editor.Count) or (ToLine = 0) then
    ToLine := Editor.Count;
  if FromLine < 1 then
    FromLine := 1;
  if (ToLine - FromLine) >= 0 then // s'il y a des lignes à analyser
  begin
    Line := FromLine-1; // départ de l'analyse
    // tant qu'il y a des lignes et qu'il n'y a pas d'erreur
    while (Line < ToLine) and (fKernelResult = C_None) do
    begin
      L1 := TGVList.Create;
      try
        // récupère la ligne en cours
        L1.Text := CBeginList + Trim(Editor[Line]) + CEndList;
        // *** c'est une ligne vide ou un commentaire ?
        if L1.IsEmptyList or (L1.First = CComment) then
        begin
          Inc(Line); // ligne suivante
        end
        else
        begin
          // *** est-ce Pour ?
          if AnsiSameText(L1.First, P_For) then
          begin
            try // si erreur : mauvaise définition
              // on extrait le nom de la procédure (après Pour !)
              if L1.Count > 1 then // un mot au moins suit ?
              begin
                Name := L1[1];
                St := CBeginList; // St est le caractère [
                // *** construit la liste des paramètres si nécessaire
                if L1.Count > 2 then
                  // on ajoute les paramètres (ni le Pour ni le nom)
                  for I := 2 to L1.Count - 1 do
                    if St = CBeginList then
                      St := St + L1[I]
                    else
                      St := St + CBlank + L1[I];
                // on stocke les paramètres
                Def := CBeginList + St + CEndList + CBlank;
                // *** on passe à la définition
                St := CBeginList;
                Inc(Line); // en changeant de ligne
                repeat
                  // on examine la ligne en cours
                  L2 := TGVList.Create;
                  try
                    L2.Text := (CBeginList + Trim(Editor[Line]) + CEndList);
                    if not L2.IsEmptyList then // on ignore une ligne vide
                    begin
                      // *** c'est le mot Fin ?
                      if AnsiSameText(L2.First, P_End) then
                      begin
                        // on clôt la liste de définition
                        Def := Def + Trim(St) + CEndList + CEndList;
                        if AddProc(Name, Def) then // enregistre la définition
                        begin // l'enregistrement s'est bien déroulé
                          fKernelResult := C_OKProc; // procédure enregistrée
                          Change; // on signale le changement
                        end;
                      end
                      else
                        St := St + CBeginList + Trim(Editor[Line]) + CEndList +
                          CBlank; // on ajoute la ligne
                    end;
                  finally
                    L2.Free; // on libère la liste de travail
                  end;
                  if (Line > ToLine) and (fKernelResult <> C_OKProc) then
                  begin
                    Error := Line;
                    ErrorMessage(C_NotEnd, Name); // mot Fin non rencontré
                  end;
                  Inc(Line);
                until (fKernelResult <> C_None); // on change de ligne
                if fKernelResult = C_OKProc then
                  // on annule le drapeau de mauvais enregistrement
                  ClearError;
              end
              else
                ErrorMessage(C_NoName, Editor[Line]); // pas de nom après Pour
            except
              Error := Line;
              ErrorMessage(C_BadDef, Name); // mauvaise définition
            end;
          end
          else
          begin
            Error := Line;
            ErrorMessage(C_NotFor, EmptyStr); // c'est une erreur : pas de Pour
          end;
        end;
      finally
        L1.Free; // on libère la liste de travail
      end;
    end;
  end
  else
    ErrorMessage(C_EmptyEdit, EmptyStr); // pas de lignes à analyser
  Result := (fKernelResult = C_None); // OK si pas d'erreur...
end;

(* ********************************************************************* *)

function TGVLogoKernel.Exists(const Name: string): Boolean;
// *** vérifie que le nom existe ***
begin
  ClearError; // rien à signaler
  Result := fWorkZone.IsListP(Name); // liste présente ?
end;

(* ********************************************************************* *)

function TGVLogoKernel.GetError: Boolean;
// *** erreur ? ***
begin
  Result := (fKernelResult <> C_None);
end;

procedure TGVLogoKernel.SetProtected(AValue: Boolean);
// *** protection ? ***
begin
  if fProtected = AValue then
    Exit; // on sort si aucun changement
  fProtected := AValue;
  Change; // on signifie le changement
end;

(* ********************************************************************* *)

function TGVLogoKernel.GetKernelResult: TGVError;
// *** résultat d'une opération ***
begin
  Result := fKernelResult; // enregistrement
  ClearError; // remise à zéro
end;

(* ********************************************************************* *)

function TGVLogoKernel.GList(const Prop: string): string;
// *** liste des objets contenant la propriété ***
var
  S: string;
begin
  ClearError; // rien à signaler
  Result := CBeginList; // début de liste
  try
    for S in fWorkZone do // on balaie la zone de travail
      if (S <> EmptyStr) and // nom de la liste
        (Prop = CExtLP) or fWorkZone.IsProp(S, Prop) then
        // cherche la propriété
        Result := Result + S + CBlank; // si OK on ajoute le nom
  finally
    Result := TrimRight(Result) + CEndList; // on ferme la liste
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsEmptyPList(const Name: string): Boolean;
// *** la liste de propriété est-elle vide ? ***
begin
  ClearError; // rien à signaler
  Result := (PListe(Name) = EmptyStr);
end;

(* ********************************************************************* *)

procedure TGVLogoKernel.Dump(Lst: TStrings);
// *** renvoie tout l'espace de travail ***
var
  I: Integer;
begin
  for I := 1 to fWorkZone.CountListP do
    Lst.Append(fWorkZone[I]);
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsBurriedPck(const Name: string): Boolean;
// *** paquet enfoui ? ***
begin
  Result := False; // on suppose que non
  ClearError; // rien à signaler
  if IsPck(Name) then // est-ce un paquet ?
    Result := fWorkZone.IsProp(Name, CBurried) // propriété enterrée ?
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsPck(const Name: string): Boolean;
// *** paquet ? ***
begin
  Result := IsObj(Name, CPackage);
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsPrim(const Name: string): Boolean;
// *** primitive ? ***
begin
  Result := (NumPrim(Name) <> -1);
  ClearError; // on annule une éventuelle erreur
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsProc(const Name: string): Boolean;
// *** procédure ? ***
begin
  Result := IsObj(Name, CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsProtected(const Name: string): Boolean;
// *** élément protégé ? ***
begin
  Result := False; // suppose que l'objet n'est pas protégé
  ClearError; // pas de message
  if Exists(Name) then // élément existant ?
  begin
    if IsPck(Name) then // si oui, est-ce un paquet ?
      Result := True // un paquet est toujours protégé
    else // sinon chercher si présent dans paquet
    begin
      if IsInPck(Name) then
        // quel paquet ? quelle protection ?
        Result := IsBurriedPck(RProp(Name, CInPackage))
      else
        Result := Protected; // sinon protection courante
    end;
  end
  else
    ErrorMessage(C_NotObject, Name) // ce n'est pas un objet
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsValid(const Name: string): Boolean;
// *** le nom est-il valide ? ***
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if Name = CComment then // on accepte les commentaires
    Result := True
  else if Name <> EmptyStr then // pas de chaîne vide !
  begin
    if Name[Length(Name)] = CAsk then // on accepte le "?" à la fin
    begin
      if Length(Name) <> 1 then // mais pas seulement !
        Result := IsValidIdent(Copy(Name, 1, Length(Name) - 1));
    end
    else if Name[1] = CDot then // on accepte le "." au début
    begin
      if Length(Name) <> 1 then // mais pas seulement !
        Result := IsValidIdent(Copy(Name, 2, Length(Name)));
    end
    else
      Result := IsValidIdent(Name); // le reste est-il valide ?
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsValidDef(const St: string): Boolean;
// *** définition valide ? ***
var
  Lst1, Lst2, Lst3: TGVList;
  I: Integer;
  T: TStrings;
  S: string;
begin
  Result := False; // on suppose une erreur
  ClearError; // pas de message
  Lst1 := TGVList.Create;
  try
    if fTempList.IsValid(St) then // liste correcte ?
      Lst1.Text := St
    else
    begin
      ErrorMessage(C_BadList, St); // mauvaise liste
      exit; // on sort de la procédure
    end;
    Lst2 := TGVList.Create; // premier élément = paramètres
    try
      T := TStringList.Create; // liste des paramètres pour doublons
      try
        try
          Lst2.Text := Lst1.First;
          for I := 0 to Lst2.Count - 1 do // on repère les paramètres
            // s'il existe déjà ou s'il est incorrect
            if (T.IndexOf(Lst2[I]) <> -1) or
              not IsValidParam(CBeginList + Lst2[I] + CEndList) then
            begin
              Result := False;
              ErrorMessage(C_BadParam, Lst2[I]); // mauvais paramètre
              break; // on sort de la boucle
            end
            else
            begin
              T.Add(Lst2[I]); // stocke le nouveau paramètre
              Result := True;
            end;
        except
          ErrorMessage(C_BadParam, St); // mauvais paramètres
        end;
      finally
        T.Free; // on libère la liste de travail
      end;
    finally
      Lst2.Free; // idem
    end;
    if Result then // si tout va bien : examinons la définition
    begin
      Lst2 := TGVList.Create; // on analyse les lignes
      try
        if (Lst1.Count > 1) then // on doit avoir une définition
        begin
          Lst3 := TGVList.Create; // on crée la liste des lignes
          try
            Lst3.Text := Lst1[1]; // on lui affecte la liste de définition
            for S in Lst3 do // on balaie les lignes
            begin
              Result := (fTempList.IsValid(S) or (S = EmptyStr));
              // liste valide ou vide
              if not Result then // sinon on sort
              begin
                ErrorMessage(C_BadDef, Lst1[1]); // mauvaise définition
                break;
              end;
            end;
          finally
            Lst3.Free;
          end;
        end
        else
          ErrorMessage(C_BadDef, St);
      finally
        Lst2.Free; // on libère la liste
      end;
    end;
  finally
    Lst1.Free; // on libère la liste de travail
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsValidParam(const Lst: string): Boolean;
// *** les paramètres sont-ils valides ? ***
var
  L: TGVList;
  T: TStrings;
  S: string;
begin
  Result := (Lst = fTempList.EmptyList); // on accepte la liste vide
  ClearError; // rien à signaler
  if not Result then // autres cas
  begin
    L := TGVList.Create; // liste de travail
    try
      L.Text := Lst;
      T := TStringList.Create; // liste de chaînes de travail
      try
        for S in L do // repère les paramètres
          if (T.IndexOf(S) <> -1) or // si existe déjà
            not IsValidVar(S) then // ou incorrect
          begin
            Result := False; // ce n'est pas bon !
            ErrorMessage(C_BadParam, S); // mauvais paramètre
            break; // on sort de la boucle
          end
          else
          begin
            T.Add(S); // stocke le nouveau paramètre
            Result := True;
          end;
      finally
        T.Free; // on libère la liste de travail
      end;
    finally
      L.Free; // libération de la liste de travail
    end;
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsValidVar(const Name: string): Boolean;
// *** nom de variable valide ? ***
begin
  Result := False; // suppose une erreur
  // chaîne non vide et premier caractère = ":" ?
  if (Name <> EmptyStr) and (Name[1] = CColon) then
    Result := IsValid(Copy(Name, 2, Length(Name))); // nom correct ?
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsVar(const Name: string): Boolean;
// *** variable ? ***
begin
  Result := IsObj(Name, CVr);
end;

(* ********************************************************************* *)

function TGVLogoKernel.ListToPck(const Name, Lst: string): Boolean;
// *** liste vers paquet ***
var
  L: TGVList;
  I: Integer;
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if fTempList.IsValid(Lst) then // liste valide ?
    begin
      L := TGVList.Create; // liste de travail
      try
        L.Text := Lst;
        I := L.Count - 1; // nombre d'éléments
        while (I >= 0) do
        begin
          // si on parvient à empaqueter l'objet
          if ToPck(Name, L[I]) then
            Dec(I) // on passe au suivant
          else
            break; // sinon c'est le dernier
        end;
        Result := (fKernelResult = C_None); // pas d'erreur ?
      finally
        L.Free; // libération de la liste de travail
      end;
    end
    else
      ErrorMessage(C_BadList, Lst); // liste incorrecte
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.Load(const FileName, Kind: string): Boolean;
// *** chargement générique ***
var
  L: TStringList;
  Lst: TGVList;
  StFile, S: string;
  I, J: Integer;
begin
  Result := False; // erreur par défaut
  ClearError; // pas de message
  L := TStringList.Create; // on crée la liste provisoire
  try
    try
      // on s'assure de la bonne extension
      if PosEx(CDot, FileName) = 0 then // si aucune extension
        StFile := FileName + Kind // on en ajoute une
      else
        StFile := FileName;
      if FileExists(StFile) then
        L.LoadFromFile(StFile) // on charge le fichier
      else
        ErrorMessage(C_FileNotFound, StFile); // fichier introuvable
      // on vérifie son contenu
      if (fKernelResult = C_None) then
      begin
        if L[0] = CHeader then // est-ce la bonne version ?
        begin
          L.Delete(0); // on élimine l'entête
          Lst := TGVList.Create;
          L.NameValueSeparator := CSep; // séparateur entre noms et valeurs
          try
            if (fKernelResult = C_None) then // on continue si pas d'erreur
            begin
              for I := 0 to L.Count - 1 do // on balaie le fichier
              begin
                S := L.Names[I]; // on a le nom
                // est-ce que l'objet est protégé ?
                if IsProtected(S) then
                begin
                  ErrorMessage(C_Protected, S); // l'objet est protégé
                  exit;
                end;
                Lst.Text := L.Values[S]; // et les propriétés
                // est-ce le type cherché ?
                if (Kind <> CExtLP) and (Lst.First <> Kind) then
                begin // le contenu est incorrect
                  ErrorMessage(C_BadContent, StFile);
                  exit;
                end;
                // est-ce que le nom est déjà pris par un objet de nature différente ?
                if Exists(S) then
                  for J := 0 to Lst.Count - 1 do
                    if not(Odd(J) or fWorkZone.IsProp(S, Lst[J])) then
                    begin
                      ErrorMessage(C_BadObj, S);
                      exit;
                    end;
                // sinon les stocke
                for J := 0 to Lst.Count - 1 do
                  if not Odd(J) then // par paires
                    if not fWorkZone.UpDateListP(S, Lst[J], Lst[J + 1]) then
                    begin
                      ErrorMessage(C_BadContent, S); // le contenu est incorrect
                      exit;
                    end;
              end;
              Result := (fKernelResult = C_None); // tout est OK ?
            end;
          finally
            Lst.Free; // on libère la liste de travail
          end;
        end
        else
          ErrorMessage(C_Version, StFile); // erreur de version
      end;
    except
      ErrorMessage(C_BadFile, StFile); // mauvais fichier
    end;
  finally
    L.Free;
    Change; // toujours signaler un changement
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.ObjCount(const Kind: string): Integer;
// *** compte des objets d'un type donné ***
var
  S: string;
begin
  Result := 0; // pas d'objets
  ClearError; // rien à signaler
  for S in fWorkZone do // on balaie la zone de travail
    if (S <> EmptyStr) and // nom de la liste
      fWorkZone.IsProp(S, Kind) then // si c'est un objet du même type
      Inc(Result); // on incrémente le résultat
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsObj(const Name, Kind: string): Boolean;
// *** est-ce un objet du type donné ? ***
begin
  ClearError; // rien à signaler
  Result := fWorkZone.IsProp(Name, Kind);
end;

(* ********************************************************************* *)

procedure TGVLogoKernel.RemoveAllLocVars;
// *** destruction de toutes les variables locales ***
begin
  fLocalVars.Clear;
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveAllObj(const Kind: string): Boolean;
// *** supression d'un type d'objet donné ***
var
  S: string;
begin
  Result := True; // suppose que tout est OK
  ClearError; // rien à signaler
  for S in fWorkZone do // on balaie la liste de travail
    // est-ce un objet de même type et non enterré ?
    if IsObj(S, Kind) and not IsBurried(S) then
    begin
      Result := fWorkZone.RemoveListP(S); // on le détruit
      if not Result then // une erreur ?
        break; // on quitte la boucle
    end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveObj(const Name, Kind: string): Boolean;
// *** supprime un objet selon son type ***
begin
  Result := False; // suppose une erreur
  ClearError; // rien à signaler
  if IsObj(Name, Kind) then // est-ce un objet du type demandé ?
  begin
    if IsProtected(Name) then // est-il protégé ?
      ErrorMessage(C_Protected, Name) // élément protégé
    else
    begin
      Result := fWorkZone.RemoveProp(Name, Kind);
      // sinon, on supprime la propriété
      if Result then
      begin
        // s'il ne reste que la propriété de paquet => on supprime l'objet
        if (fWorkZone.CountProps(Name) = 1) and
          fWorkZone.IsProp(Name, CInPackage) then
          Result := fWorkZone.RemoveListP(Name);
        Change; // changement notifié si effectif
      end;
    end;
  end
  else
    ErrorMessage(C_NotObject, Name); // ce n'est pas un objet adapté
end;

(* ********************************************************************* *)

function TGVLogoKernel.LoadAll(const FileName: string): Boolean;
// *** chargement de l'espace ***
begin
  Result := Load(FileName, CExtLP);
end;

(* ********************************************************************* *)

function TGVLogoKernel.LoadProcs(const FileName: string): Boolean;
// *** chargement des procédures ***
begin
  try
    // on charge en écrasant si nécessaire
    Result := Load(FileName, CProc);
  finally
    Change; // changement signalé
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.LoadVars(const FileName: string): Boolean;
// *** chargement de variables ***
begin
  try
    // on charge en écrasant si nécessaire
    Result := Load(FileName, CVr);
  finally
    Change; // changement signalé
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.LocVarsCount: Integer;
// *** nombre de variables locales ***
begin
  Result := fLocalVars.CountListP;
end;

(* ********************************************************************* *)

function TGVLogoKernel.LocVarsToList: string;
// *** liste des variables locales ***
var
  S: string;
begin
  Result := CBeginList;
  try
    for S in fLocalVars do
      Result := Result + S + CBlank;
  finally
    Result := TrimRight(Result) + CEndList;
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.NumParamsPrim(const Name: string): Integer;
// *** nombre de paramètres de la primitive ***
begin
  ClearError; // pas de message
  Result := NumPrim(Name); // on cherche la primitive
  if (Result <> -1) then // trouvée ?
    Result := GVPrimName[Result].NbParams // on renvoie le nombre de paramètres
  else
    ErrorMessage(C_NotPrim, Name); // ce n'est pas une primitive
end;

(* ********************************************************************* *)

function TGVLogoKernel.NumPrim(const Name: string): Integer;
// *** numéro de primitive ***
var
  I: Integer;
begin
  ClearError; // pas de message
  Result := -1; // mauvais nombre
  for I := 1 to CPrimCount do // on balaie le tableau
  begin
    if AnsiSameText(GVPrimName[I].Name, Name) then // trouvée ?
    begin
      Result := I; // on stocke le résultat
      break; // on sort de la boucle
    end;
  end;
  if Result = -1 then // non trouvée
    ErrorMessage(C_NotPrim, Name); // ce n'est pas une primitive
end;

(* ********************************************************************* *)

function TGVLogoKernel.PckAll(const Name: string): Boolean;
// *** tout dans un paquet ***
var
  S: string;
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if IsPck(Name) then // est-ce un paquet ?
  begin
    for S in fWorkZone do
    begin
      if ToPck(Name, S) then // empaquetage possible ?
        Continue
      else
      begin // objet protégé ?
        if (fKernelResult in [C_PackageForbidden, C_PrimForbidden, C_Protected])
        then
        begin
          ClearError; // ne pas en tenir compte
          Continue;
        end
        else
          break; // c'est le dernier avec erreur
      end;
    end;
    Result := (fKernelResult = C_None); // résultat de sortie
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.UnPackObj(const Name: string): Boolean;
// *** dépaquette un objet ***
begin
  Result := False; // suppose une erreur
  if Exists(Name) then // l'objet existe-t-il ?
  begin
    if IsInPck(Name) then // est-il dans un paquet ?
    begin
      if IsBurriedPck(fWorkZone.ValProp(Name, CInPackage)) then
        // paquet enfoui ?
        ErrorMessage(C_Burried, Name)
      else
      begin
        Result := fWorkZone.RemoveProp(Name, CInPackage);
        // on enelève la propriété
      end
    end
    else
      ErrorMessage(C_NotInPackage, Name); // pas dans un paquet
  end
  else
    ErrorMessage(C_NotObject, Name); // objet inconnu
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsInPck(const Name: string): Boolean;
// *** l'objet est-il dans un paquet ? ***
begin
  Result := False; // erreur par défaut
  ClearError; // pas de message d'erreur
  if Exists(Name) then
    Result := fWorkZone.IsProp(Name, CInPackage) // recherche de la propriété
  else
    ErrorMessage(C_NotObject, Name); // objet inconnu
end;

(* ********************************************************************* *)
function TGVLogoKernel.IsLocVar(const Name: string): Boolean;
// *** l'objet est-il une variable locale ? ***
begin
  Result := fLocalVars.IsListP(Name);
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsBurried(const Name: string): Boolean;
// *** l'objet est-il enterré ? ***
begin
  Result := IsBurriedPck(BelongsTo(Name)); // recherche de la propriété
end;

(* ********************************************************************* *)

function TGVLogoKernel.BelongsTo(const Name: string): string;
// *** à quel paquet appartient l'objet ? ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  ClearError; // pas de message d'erreur
  if Exists(Name) then // l'objet existe-t-il ?
  begin
    if IsInPck(Name) then // est-il dans un paquet ?
      Result := fWorkZone.ValProp(Name, CInPackage) // si oui on cherche lequel
    else
      ErrorMessage(C_NotInPackage, Name); // pas dans un paquet
  end
  else
    ErrorMessage(C_NotObject, Name); // objet inconnu
end;

(* ********************************************************************* *)

function TGVLogoKernel.PcksCount: Integer;
// *** décompte du nombre de paquets ***
begin
  Result := ObjCount(CPackage);
end;

(* ********************************************************************* *)

function TGVLogoKernel.PcksToList: string;
// *** liste des paquets ***
begin
  Result := GList(CPackage);
end;

(* ********************************************************************* *)

function TGVLogoKernel.PckToEdit(const Name: string; Lst: TStrings): Boolean;
// *** paquet vers l'éditeur ***
var
  S: string;
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if IsPck(Name) then // est-ce un paquet ?
  begin
    for S in fWorkZone do // on balaie la zone de travail
    begin
      if (S <> EmptyStr) and // nom de la liste
      // on ajoute si procédure et propriété présentes et correctes
        IsProc(S) and IsInPck(S) and
          AnsiSameText(fWorkZone.ValProp(S, CInPackage), Name) then
      begin
        Result := ProcToEdit(S, Lst); // on ajoute la procédure
        if not Result then
          break; // si erreur on sort de la boucle
      end;
      Application.ProcessMessages; // on traite les messages
    end;
  end
  else
    ErrorMessage(C_NotPackage, Name); // pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.PckToList(const Name: string): string;
// *** liste du paquet ***
var
  S: string;
begin
  Result := CBeginList; // on ouvre la liste
  ClearError; // pas de message
  try
    if IsPck(Name) then // est-ce un paquet ?
    begin
      for S in fWorkZone do // on balaie la zone de travail
        if (S <> EmptyStr) and // liste récupérée
        // si c'est la propriété paquet et si elle correspond à Name
          IsInPck(S) and AnsiSameText(fWorkZone.ValProp(S, CInPackage), Name)
        then
          Result := Result + S + CBlank; // on ajoute le nom
    end
    else
      ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
    Result := TrimRight(Result);
  finally
    Result := Result + CEndList; // on ferme la liste
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.ParamsLine(const Name: string): string;
// *** ligne de paramètres d'une procédure ***
var
  Lst: TGVList;
begin
  ClearError; // pas de message
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create;
    try
      Lst.Text := RProp(Name, CProc); // création
      Result := Lst.First; // recherche des paramètres
    finally
      Lst.Free; // libère la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ParamsLine(const Name: string;
  out ParLine: string): Boolean;
// *** ligne de paramètres d'une procédure ***
var
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
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
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ParamNum(const Name: string; Num: Integer): string;
// *** renvoie un paramètre d'une procédure ***
var
  S: string;
  Lst: TGVList;
begin
  ClearError; // pas de message
  if IsProc(Name) then // est-ce une procédure ?
  begin
    if (Num <= ParamsCount(Name)) and (Num >= 1) then
    begin
      S := ParamsLine(Name); // ligne des paramètres
      Lst := TGVList.Create; // liste de travail
      try
        Lst.Text := S;
        Result := Lst[Num-1]; // paramètre si possible
      finally
        Lst.Free; // libération de la liste de travail
      end;
    end
    else
      ErrorMessage(C_BadParam, IntToStr(Num)); // mauvais paramètre
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ParamNum(const Name: string; Num: Integer;
  out ParNum: string): Boolean;
// *** renvoie un paramètre d'une procédure ***
var
  S: string;
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if IsProc(Name) then // est-ce une procédure ?
  begin
    if (Num <= ParamsCount(Name)) and (Num >= 1) then
    begin
      S := ParamsLine(Name); // ligne des paramètres
      Lst := TGVList.Create; // liste de travail
      try
        Lst.Text := S;
        ParNum := Lst[Num-1]; // paramètre si possible
        Result := True;
      finally
        Lst.Free; // libération de la liste de travail
      end;
    end
    else
      ErrorMessage(C_BadParam, IntToStr(Num)); // mauvais paramètre
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ParamsCount(const Name: string): Integer;
// *** nombre de paramètres d'une procédure ***
var
  Lst: TGVList;
begin
  Result := -1; // mauvais nombre
  ClearError; // pas de message
  if IsProc(Name) then // est-ce une procédure ?
  begin
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := ParamsLine(Name);
      Result := Lst.Count; // nombre d'éléments
      if Lst.First = EmptyStr then
        Dec(Result);
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.PListe(const Name: string): string;
// *** liste associée à une propriété ***
begin
  ClearError; // rien à signaler
  Result := fWorkZone.ValListP(Name); // renvoie la liste
  if Result = EmptyStr then // ne peut être une chaîne vide
    ErrorMessage(C_NotLProp, Name); // pas une propriété
end;

(* ********************************************************************* *)

function TGVLogoKernel.PListe(const Name: string; out Value: string): Boolean;
// *** liste associée à une propriété ***
begin
  Result := False; // suppose une erreur
  ClearError; // rien à signaler
  Value := fWorkZone.ValListP(Name); // renvoie la liste
  if Value = EmptyStr then // ne peut être une chaîne vide
    ErrorMessage(C_NotLProp, Name) // pas une propriété
  else
    Result := True; // tout est OK
end;

(* ********************************************************************* *)

function TGVLogoKernel.PrimByNum(const N: Integer): string;
// *** primitive par son numéro ***
begin
  if (N > 0) and (N <= CPrimCount) then // dans les bornes ?
    Result := GVPrimName[N].Name // recherche du nom de la primitive
  else
    ErrorMessage(C_NotPrim, IntToStr(N)); // non !
end;

(* ********************************************************************* *)

function TGVLogoKernel.PrimByNum(const N: Integer;
  out PrimBNum: string): Boolean;
// *** primitive par son numéro ***
begin
  Result := False; // suppose une erreur
  if (N > 0) and (N <= CPrimCount) then // dans les bornes ?
  begin
    PrimBNum := GVPrimName[N].Name; // recherche du nom de la primitive
    Result := True;
  end
  else
    ErrorMessage(C_NotPrim, IntToStr(N)) // non !
end;

(* ********************************************************************* *)

function TGVLogoKernel.PrimsCount: Integer;
// *** décompte du nombre de primitives ***
begin
  Result := CPrimCount;
end;

(* ********************************************************************* *)

function TGVLogoKernel.PrimsToList: string;
// *** liste des primitives ***
var
  S: GVPrimRec;
begin
  Result := CBeginList; // début de liste
  try
    for S in GVPrimName do // on balaie le tableau
      Result := Result + S.Name + ' '; // on ajoute le nom trouvé
  finally
    Result := TrimRight(Result) + CEndList; // fin de liste
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcLine(const Name: string; Line: Integer): string;
// *** une ligne particulière d'une procédure ***
var
  S: string;
  Lst: TGVList;
begin
  ClearError; // rien à signaler
  Result := EmptyStr; // chaîne vide par défaut
  if IsProc(Name) then // est-ce une procédure ?
  begin
    S := ProcListDef(Name); // recherche de la définition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      if (Line > 0) and (Line <= Lst.Count) then
        Result := Lst[Line-1] // paramètre si possible
      else
        ErrorMessage(C_BadLine, IntToStr(Line)); // mauvaise ligne
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcLine(const Name: string; Line: Integer;
  out PrLine: string): Boolean;
// *** une ligne particulière d'une procédure ***
var
  S: string;
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  ClearError; // rien à signaler
  if IsProc(Name) then // est-ce une procédure ?
  begin
    S := ProcListDef(Name); // recherche de la définition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      if (Line >= 0) and (Line < Lst.Count) then
      begin
        PrLine := Lst[Line]; // paramètre si possible
        Result := True; // tout est OK
      end
      else
        ErrorMessage(C_BadLine, IntToStr(Line)); // mauvaise ligne
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcLinesCount(const Name: string): Integer;
// *** nombre de lignes d'une définition ***
var
  S: string;
  Lst: TGVList;
begin
  Result := -1; // mauvais nombre
  ClearError; // rien à signaler
  if IsProc(Name) then // est-ce une procédure ?
  begin
    S := ProcListDef(Name); // définition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      Result := Lst.Count; // nombre d'éléments
      if Lst.First = EmptyStr then
        Dec(Result);
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcListDef(const Name: string): string;
// *** liste de définition d'une procédure ***
var
  S: string;
  Lst: TGVList;
begin
  ClearError; // rien à signaler
  if IsProc(Name) then // est-ce une procédure ?
  begin
    S := RProp(Name, CProc); // définition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      Result := Lst.Last; // recherche de la définition
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcListDef(const Name: string;
  out PrListDef: string): Boolean;
// *** liste de définition d'une procédure ***
var
  S: string;
  Lst: TGVList;
begin
  Result := False;
  ClearError; // rien à signaler
  if IsProc(Name) then // est-ce une procédure ?
  begin
    S := RProp(Name, CProc); // définition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      PrListDef := Lst.Last; // recherche de la définition
      Result := True; // tout est OK
    finally
      Lst.Free; // libération de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcsCount: Integer;
// *** décompte du nombre de procédures ***
begin
  Result := ObjCount(CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcsToList: string;
// *** liste des procédures ***
begin
  Result := GList(CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcToEdit(const Name: string; Lst: TStrings): Boolean;
// *** procédure vers éditeur ***
var
  I, J: Integer;
  S: string;
begin
  Result := False; // suppose une erreur
  ClearError; // rien à signaler
  if IsProc(Name) then // la procédure existe-t-elle ?
  begin
    S := EmptyStr; // chaîne vide
    J := ParamsCount(Name); // nombre de paramètres
    if (J <> 0) then // si au moins un paramètre
      for I := 1 to J do // les recherche
        S := S + CBlank + ParamNum(Name, I); // paramètres
    Lst.Add(P_For + CBlank + Name + S); // entête
    J := ProcLinesCount(Name); // nombre de lignes
    if (J <> 0) then // s'il y a au moins une ligne
      for I := 1 to J do // les recherche et les ajoute
        Lst.Add(CBlank + CBlank + fTempList.ListToStr(ProcLine(Name, I)));
    Lst.Add(P_End); // fin de la procédure
    Lst.Add(EmptyStr); // espace
    Result := True; // tout est OK
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une procédure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcsToEdit(const LstP: string; Lst: TStrings): Boolean;
// *** envoie une liste de procédures vers un éditeur ***
var
  S: string;
  L: TGVList;
begin
  Result := False; // suppose une erreur
  L := TGVList.Create;
  try
    if fTempList.IsValid(LstP) then // la liste est-elle valide ?
    begin
      L.Text := LstP; // si oui on l'affecte à celle de travail
      for S in L do // on balaie les valeurs
      begin
        Result := ProcToEdit(S, Lst); // on envoie une procédure
        if not Result then // on sort en cas d'erreur
          break;
      end;
    end
    else
      ErrorMessage(C_BadList, LstP); // liste invalide
  finally
    L.Free;
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveAllProcs: Boolean;
// *** détruit toutes les procédures ***
begin
  Result := RemoveAllObj(CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveAllVars: Boolean;
// *** détruit toutes les variables ***
begin
  Result := RemoveAllObj(CVr);
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveLocVar(const Name: string): Boolean;
// *** destruction d'une varialbe locale
begin
  Result := fLocalVars.RemoveListP(Name);
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemovePck(const Name: string): Boolean;
// *** destruction d'un paquet ***
var
  S: string;
begin
  Result := False; // on suppose une erreur
  ClearError; // rien à signaler
  if IsPck(Name) then // est-ce un paquet ?
  begin
    Result := True; // tout va bien
    for S in fWorkZone do // on balaie la zone de travail
    begin
      if (S <> EmptyStr) and
      // cherche la propriété paquet
        AnsiSameText(BelongsTo(S), Name) then
      begin
        Result := fWorkZone.RemoveProp(S, CInPackage);
        // on supprime la propriété
        if not Result then
          break; // on sort de la boucle si erreur
      end;
    end;
    if Result then // si tout est Ok
    begin
      Result := fWorkZone.RemoveListP(Name); // on supprime le paquet
      if Result then
        Change; // changement notifié si effectif
    end;
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveProc(const Name: string): Boolean;
// *** suppression d'une procédure ***
begin
  Result := RemoveObj(Name, CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveSomeObj(const Lst, Kind: string): Boolean;
// *** suppression d'une liste d'objets ***
var
  L: TGVList;
  I: Integer;
  S: string;
begin
  Result := False; // on suppose une erreur
  ClearError; // rien à signaler
  L := TGVList.Create; // liste des objets à supprimer
  try
    L.Text := Lst; // on construit la liste des objets à supprimer
    I := L.Count - 1; // on compte les éléments
    if I = -1 then
      ErrorMessage(C_EmptyList, Lst); // signalement d'une liste vide
    while (I >= 0) do
    begin
      // on vérifie l'existence de l'objet
      Result := fWorkZone.IsProp(L[I], Kind);
      if not Result then
      begin
        ErrorMessage(C_BadList, Lst); // ce n'est pas une bonne liste
        break; // on sort de la boucle
      end;
      Dec(I); // suivant
    end;
    if Result then // si tout va bien, on supprime les objets
      for S in L do
      begin
        Result := RemoveObj(S, Kind);
        if not Result then
        begin
          ErrorMessage(C_Inc, S); // erreur inconnue
          break;
        end;
      end;
  finally
    L.Free; // idem
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveSomeProcs(const Lst: string): Boolean;
// destruction d'une liste de procédures
begin
  Result := RemoveSomeObj(Lst, CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveSomeVars(const Lst: string): Boolean;
// destruction d'une liste de variables
begin
  Result := RemoveSomeObj(Lst, CVr);
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveVar(const Name: string): Boolean;
// *** destruction d'une variable ***
begin
  Result := RemoveObj(Name, CVr);
end;

(* ********************************************************************* *)

function TGVLogoKernel.RProp(const Name, Prop: string): string;
// *** valeur d'une propriété ***
begin
  ClearError; // rien à signaler
  Result := EmptyStr; // chaîne vide par défaut
  if Exists(Name) then // nom existe ?
  begin
    if fWorkZone.IsProp(Name, Prop) then
      Result := fWorkZone.ValProp(Name, Prop) // renvoie la valeur
    else
      ErrorMessage(C_NotProp, Prop); // propriété inconnue
  end
  else
    ErrorMessage(C_NotLProp, Name); // liste inconnue
end;

(* ********************************************************************* *)

function TGVLogoKernel.RProp(const Name, Prop: string;
  out Value: string): Boolean;
// *** valeur d'une propriété ***
begin
  Result := False; // suppose une erreur
  ClearError; // rien à signaler
  if Exists(Name) then // nom existe ?
  begin
    if fWorkZone.IsProp(Name, Prop) then
    begin
      Value := fWorkZone.ValProp(Name, Prop); // renvoie la valeur
      Result := True;
    end
    else
      ErrorMessage(C_NotProp, Prop); // propriété inconnue
  end
  else
    ErrorMessage(C_NotLProp, Name); // liste inconnue
end;

(* ********************************************************************* *)

function TGVLogoKernel.Save(const FileName, Lst, Kind: string): Boolean;
// *** sauvegarde générique ***
var
  LstPL: TGVPropList;
  L1, L2: TGVList;
  I, J: Integer;
begin
  Result := False; // on suppose une erreur
  ClearError; // rien à signaler
  LstPL := TGVPropList.Create; // liste de propriétés provisoire créée
  L1 := TGVList.Create; // liste des objets à sauver
  try
    L1.Text := Lst; // on construit la liste des objets à sauver
    I := L1.Count - 1; // on compte les éléments
    if I = -1 then
      ErrorMessage(C_EmptyList, Lst); // signalement d'une liste vide
    L2 := TGVList.Create;
    try
      while (I >= 0) do // on transfère uniquement les objets existants
      begin
        if ((Kind = CExtLP) and Exists(L1[I])) or fWorkZone.IsProp(L1[I], Kind)
        then // si trouvé on mémorise
        begin
          L2.Text := fWorkZone.ValListP(L1[I]); // on recherche les propriétés
          for J := 0 to L2.Count - 1 do
            // pas le drapeau paquet
            if not(Odd(J) or (L2[J] = CInPackage) or (L2[J] = CPackage)) then
              Result := LstPL.UpDateListP(L1[I], L2[J], L2[J + 1]);
          if not Result then
            exit; // on sort en cas d'erreur
        end
        else
        begin
          Result := False; // erreur !
          ErrorMessage(C_BadList, Lst); // ce n'est pas une bonne liste
          break; // on sort de la boucle
        end;
        Dec(I); // suivant
      end;
    finally
      L2.Free;
    end;
    if Result then // si tout va bien, on sauvegarde
      try // on sauvegarde les objets avec la bonne extension
        LstPL.SaveToFile(ChangeFileExt(FileName, Kind));
      except // si erreur
        ErrorMessage(C_BadSave, ChangeFileExt(FileName, Kind)); // la signaler
        Result := False;
      end;
  finally
    LstPL.Free; // libérer la liste provisoire
    L1.Free; // idem
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.SaveAll(const FileName: string): Boolean;
// *** on sauvegarde l'espace de travail ***
begin
  Result := Save(FileName, GList(CExtLP), CExtLP);
end;

(* ********************************************************************* *)

function TGVLogoKernel.SaveAllProcs(const FileName: string): Boolean;
// *** sauve toutes les procédures ***
begin
  Result := Save(FileName, GList(CProc), CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.SaveAllVars(const FileName: string): Boolean;
// *** sauvegarde de toutes les variables ***
begin
  Result := Save(FileName, GList(CVr), CVr);
end;

(* ********************************************************************* *)

function TGVLogoKernel.SavePck(const Name: string): Boolean;
// *** sauvegarde d'un paquet ***
var
  S, St: string;
begin
  Result := False; // suppose une erreur
  ClearError; // rien à signaler
  if IsPck(Name) then // est-ce un paquet ?
  begin
    St := CBeginList; // début de liste
    try
      for S in fWorkZone do
        if IsInPck(S) and SameText(BelongsTo(S), Name) then
          St := St + S + CBlank; // si OK on ajoute le nom
    finally
      St := TrimRight(St) + CEndList; // on ferme la liste
    end;
    Result := Save(Name, St, CInPackage);
    // le nom du paquet est celui du fichier
  end
  else
    ErrorMessage(C_NotPackage, Name); // erreur : le paquet n'existe pas
end;

(* ********************************************************************* *)

function TGVLogoKernel.SaveProcs(const FileName, Lst: string): Boolean;
// *** sauve une liste de procédures ***
begin
  Result := Save(FileName, Lst, CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.SaveVars(const FileName, Lst: string): Boolean;
// *** sauve une liste de variables ***
begin
  Result := Save(FileName, Lst, CVr);
end;

(* ********************************************************************* *)

function TGVLogoKernel.TestExistsObj(const Name: string): Boolean;
// *** émet une erreur si l'objet n'existe pas
begin
  ClearError; // pas d'erreur a priori
  Result := Exists(Name);
  if not Result then
    ErrorMessage(C_NotObject, Name); // objet inconnu;
end;

(* ********************************************************************* *)

function TGVLogoKernel.ToList: string;
// *** liste des objets de la zone de travail ***
var
  S: string;
begin
  ClearError; // rien à signaler
  Result := CBeginList; // on ouvre la liste
  try
    for S in fWorkZone do
      if (S <> EmptyStr) then
        Result := Result + S + CBlank; // on ajoute le nom au résultat
  finally
    Result := TrimRight(Result) + CEndList; // on ferme la liste
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.ToPck(const Name, Obj: string): Boolean;
// *** insère un objet dans un paquet ***
begin
  Result := False; // on suppose une erreur
  ClearError; // rien à signaler
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if Exists(Obj) then // l'objet existe-t-il ?
    begin
      if IsPck(Obj) then // est-ce un paquet ?
        ErrorMessage(C_PackageForbidden, Name) // ne peut être empaqueté
      else
      begin
        // une primitive non plus 
        if IsPrim(Obj) then
          ErrorMessage(C_PrimForbidden, Obj)
        else
        begin
          if IsProtected(Obj) then // objet protégé ?
            ErrorMessage(C_Protected, Obj)
          else
          begin
            Result := DProp(Obj, CInPackage, Name); // mise à jour
            Change; // changement signalé
          end;
        end;
      end;
    end
    else
      ErrorMessage(C_NotObject, Obj); // objet inconnu;
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.UnBurryPck(const Name: string): Boolean;
// déterre un paquet
begin
  Result := False; // on suppose une erreur
  ClearError; // rien à signaler
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if IsBurriedPck(Name) then // enterré ?
    begin
      Result := fWorkZone.RemoveProp(Name, CBurried); // on le déterre
      if Result then
        Change; // changement notifié
    end
    else
      ErrorMessage(C_NotBurried, Name); // paquet non enterré
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.ValVar(const Name: string; out Value: string): Boolean;
// *** valeur d'une variable ***
begin
  Result := False; // suppose une erreur
  ClearError; // rien à signaler
  if IsVar(Name) then // est-ce une variable ?
  begin
    Value := fTempList.ListToStr(RProp(Name, CVr)); // on rend sa valeur
    Result := True; // tout est OK
  end
  else
    ErrorMessage(C_NotVar, Name); // ce n'est pas une variable
end;

(* ********************************************************************* *)

function TGVLogoKernel.ValLocVar(const Name: string; out Value: string)
  : Boolean;
// *** valeur d'une variable locale ***
begin
  ClearError; // rien à signaler
  Result := False;
  if IsLocVar(Name) then // est-ce une variable locale ?
  begin
    Value := fTempList.ListToStr(fLocalVars.ValProp(Name, CVr)); // on rend sa valeur
    Result := True;
  end
  else
    ErrorMessage(C_NotVar, Name); // ce n'est pas une variable
end;

(* ********************************************************************* *)

function TGVLogoKernel.ValLocVar(const Name: string): string;
// *** valeur d'une varialbe locale ***
begin
  ClearError; // rien à signaler
  if IsLocVar(Name) then // est-ce une variable locale ?
    Result := fTempList.ListToStr(fLocalVars.ValProp(Name, CVr)) // on rend sa valeur
  else
    ErrorMessage(C_NotVar, Name); // ce n'est pas une variable
end;

(* ********************************************************************* *)

function TGVLogoKernel.ValVar(const Name: string): string;
// *** valeur d'une variable ***
begin
  ClearError; // rien à signaler
  if IsVar(Name) then // est-ce une variable ?
    Result := fTempList.ListToStr(RProp(Name, CVr)) // on rend sa valeur
  else
    ErrorMessage(C_NotVar, Name); // ce n'est pas une variable
end;

(* ********************************************************************* *)

function TGVLogoKernel.VarsCount: Integer;
// *** nombre de variables ***
begin
  Result := ObjCount(CVr);
end;

(* ********************************************************************* *)

function TGVLogoKernel.VarsToList: string;
// *** liste des variables ***
begin
  Result := GList(CVr);
end;

end.
