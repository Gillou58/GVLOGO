{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Noyau de GVLOGO                         |
  |                  Unit� : GVKernel.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR                              |
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
// Unit� du noyau de GVLOGO : gestion de l'espace m�moire
//
// ##############################################################
//
// Le noyau permet de g�rer l'espace m�moire d�di� � GVLOGO. En particulier,
// sont trait�s les variables, les proc�dures, les primitives, les paquets, les
// listes de propri�t�s, les propri�t�s et divers objets.
//
// Le noyau est un des �l�ments fondamentaux
// de l'interpr�teur de GVLOGO.
//

interface

uses Classes,
  GVConsts,
  GVPropLists,
  GVLists;

type
  // *** classe pour le noyau

  { �v�nement lors d'une erreur }
  TGVKernelEvent = procedure(const Err: TGVError; Message: string) of object;

  { TGVLogoKernel }

  TGVLogoKernel = class(TObject)
  private
    fProtected: Boolean; // drapeau de protection
    fWorkZone: TGVPropList; // zone de travail
    fLocalVars: TGVPropList; // zone des variables locales
    fKernelResult: TGVError; // num�ro d'erreur
    fOnChange: TNotifyEvent; // notification des changements
    fOnKernelError: TGVKernelEvent; // notification d'une erreur
    fTempList: TGVListUtils; // liste de travail
    function GetKernelResult: TGVError; // r�sultat d'une op�ration
    function GetError: Boolean; // erreur ?
    procedure SetProtected(AValue: Boolean); // protection des donn�es
  protected
    // gestion des changements
    procedure Change; dynamic;
    // remet � z�ro les erreurs
    procedure ClearError; inline;
    // sauvegarde g�n�rique
    function Save(const FileName, Lst, Kind: string): Boolean;
    // chargement g�n�rique
    function Load(const FileName, Kind: string): Boolean;
    // compte d'objets d'un type donn�
    function ObjCount(const Kind: string): Integer;
    // est-ce un type d'objet donn� ?
    function IsObj(const Name, Kind: string): Boolean;
    // supression d'un type d'objet donn�
    function RemoveAllObj(const Kind: string): Boolean;
    // supression d'un objet
    function RemoveObj(const Name, Kind: string): Boolean;
    // suppression d'une liste d'objets
    function RemoveSomeObj(const Lst, Kind: string): Boolean;
  public
    // *** gestion ***

    // cr�ation
    constructor Create;
    // destruction
    destructor Destroy; override;
    // remise � z�ro de l'espace de travail
    procedure Clear;
    // message d'erreur
    procedure ErrorMessage(const Err: TGVError; Title: string);

    // *** traitement des variables ***

    // est-ce une variable?
    function IsVar(const Name: string): Boolean;
    // nombre de variables
    function VarsCount: Integer;
    // affectation � une variable
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
    // affectation � une variable locale
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

    // *** traitement des proc�dures ***

    // est-ce une proc�dure ?
    function IsProc(const Name: string): Boolean;
    // renvoie le nombre de proc�dures enregistr�es
    function ProcsCount: Integer;
    // renvoie la liste des proc�dures
    function ProcsToList: string;
    // enregistre une proc�dure
    function AddProc(const Name, Lst: string): Boolean;
    // supprime une proc�dure
    function RemoveProc(const Name: string): Boolean;
    // destruction d'une liste de proc�dures
    function RemoveSomeProcs(const Lst: string): Boolean;
    // supprime toutes les proc�dures
    function RemoveAllProcs: Boolean;
    // renvoie le nombre de param�tres d'une proc�dure
    function ParamsCount(const Name: string): Integer;
    // renvoie la liste des param�tres
    function ParamsLine(const Name: string): string; overload;
    function ParamsLine(const Name: string; out ParLine: string)
      : Boolean; overload;
    // renvoie un param�tre par son num�ro
    function ParamNum(const Name: string; Num: Integer): string; overload;
    function ParamNum(const Name: string; Num: Integer; out ParNum: string)
      : Boolean; overload;
    // renvoie le nombre de lignes du corps d'une proc�dure
    function ProcLinesCount(const Name: string): Integer;
    // renvoie une ligne du corps de la proc�dure sp�cifi�e
    function ProcLine(const Name: string; Line: Integer): string; overload;
    function ProcLine(const Name: string; Line: Integer; out PrLine: string)
      : Boolean; overload;
    // renvoie la d�finition d'une proc�dure
    function ProcListDef(const Name: string): string; overload;
    function ProcListDef(const Name: string; out PrListDef: string)
      : Boolean; overload;
    // envoie la proc�dure vers un �diteur
    function ProcToEdit(const Name: string; Lst: TStrings): Boolean;
    // envoie une liste de proc�dures vers l'�diteur
    function ProcsToEdit(const LstP: string; Lst: TStrings): Boolean;
    // envoie toutes les proc�dures vers l'�diteur
    function AllProcsToEdit(Lst: TStrings): Boolean;
    // envoie d'un �diteur vers des proc�dures
    function EditToProc(Editor: TStrings; FromLine, ToLine: Integer;
      out Error: Integer): Boolean;
    // charge des proc�dures
    function LoadProcs(const FileName: string): Boolean;
    // sauve des proc�dures
    function SaveProcs(const FileName, Lst: string): Boolean;
    // sauvegarde toutes les proc�dures
    function SaveAllProcs(const FileName: string): Boolean;
    // d�finition valide ?
    function IsValidDef(const St: string): Boolean;
    // param�tre valide ?
    function IsValidParam(const Lst: string): Boolean;

    // *** traitement des primitives ***

    // l'objet est-il une primitive ?
    function IsPrim(const Name: string): Boolean;
    // nombre de primitives
    function PrimsCount: Integer;
    // num�ro de primitive
    function NumPrim(const Name: string): Integer;
    // nombre de param�tres d'une primitive
    function NumParamsPrim(const Name: string): Integer;
    // liste des primitives
    function PrimsToList: string;
    // primitive par num�ro
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
    // d�terre un paquet
    function UnBurryPck(const Name: string): Boolean;
    // empaquette un objet
    function ToPck(const Name, Obj: string): Boolean;
    // empaquette une liste
    function ListToPck(const Name, Lst: string): Boolean;
    // renvoie la liste des objets d'un paquet
    function PckToList(const Name: string): string;
    // renvoie le nombre d'�l�ments d'un paquet
    function CountItemsPck(const Name: string): Integer;
    // renvoie la liste des paquets
    function PcksToList: string;
    // cr�e d'un paquet
    function CreatePck(const Name: string): Boolean;
    // d�truit un paquet
    function RemovePck(const Name: string): Boolean;
    // sauvegarde un paquet
    function SavePck(const Name: string): Boolean;
    // paquet vers �diteur
    function PckToEdit(const Name: string; Lst: TStrings): Boolean;
    // empaquette tout
    function PckAll(const Name: string): Boolean;
    // d�paquette un objet
    function UnPackObj(const Name: string): Boolean;
    // l'objet appartient-il � un paquet ?
    function IsInPck(const Name: string): Boolean;
    // l'objet est-il enterr� ?
    function IsBurried(const Name: string): Boolean;
    // � quel paquet appartient un objet ?
    function BelongsTo(const Name: string): string; overload;
    function BelongsTo(const Name: string; out Which: string): Boolean;
      overload;

    // *** listes de propri�t�s ***

    // d�finition d'une propri�t�
    function DProp(const Name, Prop, Value: string): Boolean;
    // valeur d'une propri�t�
    function RProp(const Name, Prop: string): string; overload;
    function RProp(const Name, Prop: string; out Value: string)
      : Boolean; overload;
    // annulation d'une propri�t�
    function AnProp(const Name, Prop: string): Boolean;
    // liste associ�e � une propri�t�
    function PListe(const Name: string): string; overload;
    function PListe(const Name: string; out Value: string): Boolean; overload;
    // liste des objets contenant la propri�t�
    function GList(const Prop: string): string;
    // la liste de propri�t�s est-elle la liste vide ?
    function IsEmptyPList(const Name: string): Boolean;

    // *** objets g�n�raux ***

    // rend tout l'espace de travail
    procedure Dump(Lst: TStrings);
    // l'objet existe-t-il ?
    function Exists(const Name: string): Boolean;
    // erreur si l'objet n'existe pas
    function TestExistsObj(const Name: string): Boolean;
    // l'objet est-il prot�g� ?
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

    // *** propri�t�s ***

    // erreur ?
    property Error: Boolean read GetError;
    // num�ro de l'erreur ?
    property KernelResult: TGVError read GetKernelResult default C_None;
    // protection ?
    property Protected: Boolean read fProtected write SetProtected default False;
    // �v�nement si changement
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    // �v�nement si erreur
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
    // on essaie de cr�er ou de mettre � jour la variable locale
    Result := fLocalVars.UpDateListP(Name, CVr, CBeginList + Value + CEndList);
    if Result then
      Change; // notifie le changement si effectif
  end
  else
    ErrorMessage(C_BadName, Name); // nom incorrect
end;

(* ********************************************************************* *)

function TGVLogoKernel.AddProc(const Name, Lst: string): Boolean;
// *** ajout d'une proc�dure ***

  procedure StoreProc;
  // enregistre une proc�dure
  begin
    Result := DProp(Name, CProc, Lst); // d�finition
    if Result then
      Change; // notifie le changement si ajout effectif
  end;

begin
  Result := False; // on suppose une erreur
  ClearError; // rien � signaler
  if Exists(Name) then // si le nom existe
  begin
    if IsProc(Name) then // si oui, est-ce une proc�dure ?
    begin
      if IsProtected(Name) then // est-elle prot�g�e ?
        ErrorMessage(C_Protected, Name) // erreur
      else if IsValidDef(Lst) then // mise � jour si correcte
        StoreProc; // enregistre la proc�dure
    end
    else // ce n'est pas une proc�dure
    begin
      if IsProtected(Name) then // l'objet est-il prot�g� ?
        ErrorMessage(C_Protected, Name) // erreur
      else
      begin
        if IsValidDef(Lst) and // d�finition valide ?
          fWorkZone.RemoveListP(Name) then // on le d�truit
            StoreProc; // enregistre la proc�dure
      end;
    end;
  end
  else // le nom n'existe pas
  begin
    if IsValid(Name) then // nom correct ?
    begin
      if IsValidDef(Lst) then // d�finition correcte ?
        StoreProc; // enregistre la proc�dure
    end
    else
      ErrorMessage(C_BadName, Name); // nom incorrect
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.AddVar(const Name, Value: string): Boolean;
// *** ajout d'une variable ou mise � jour ***
begin
  Result := False; // on suppose une erreur
  ClearError; // pas de message
  if Exists(Name) then // le nom est-il pris ?
  begin
    if IsVar(Name) then // est-ce une variable ?
    begin
      if IsProtected(Name) then // prot�g�e ?
        ErrorMessage(C_Protected, Name) // si oui, erreur
      else
      begin
        // sinon la mettre � jour
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
      // cr�er la variable
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
// envoie toutes les proc�dures vers l'�diteur
begin
  Result := ProcsToEdit(GList(CProc), Lst);
end;

(* ********************************************************************* *)

function TGVLogoKernel.AnProp(const Name, Prop: string): Boolean;
// *** annulation d'une propri�t� ***
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if Exists(Name) then // est-ce une liste ?
  begin
    if IsPrim(Name) then // est-ce une primitive ?
      ErrorMessage(C_PrimForbidden, Name) // on l'�pargne
    else
    begin
      Result := fWorkZone.RemoveProp(Name, Prop); // d�truire la propri�t� li�e
      if Result then
      begin
        if IsEmptyPList(Name) then // la liste finale est-elle vide ?
          Result := fWorkZone.RemoveListP(Name); // on supprime la liste
      end
      else
        ErrorMessage(C_NotProp, Prop); // sinon propri�t� inconnue
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
// *** � quel paquet appartient l'objet ? ***
begin
  Result := False; // erreur par d�faut
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
    if IsBurriedPck(Name) then // d�j� enterr� ?
      ErrorMessage(C_AlreadyBurried, Name) // erreur := d�j� enterr�
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
    fOnChange(Self); // on l'ex�cute
end;

(* ********************************************************************* *)

procedure TGVLogoKernel.ErrorMessage(const Err: TGVError; Title: string);
// *** message si erreur ***
begin
  fKernelResult := Err; // on m�morise l'erreur
  if Assigned(fOnKernelError) then // si le gestionnaire existe
    fOnKernelError(Err, Title); // on l'ex�cute
end;

(* ********************************************************************* *)

procedure TGVLogoKernel.ClearError;
// *** remet � z�ro les erreurs ***
begin
  fKernelResult := C_None;
  ErrorMessage(C_None, EmptyStr);
end;

(* ********************************************************************* *)

procedure TGVLogoKernel.Clear;
// *** remise � z�ro du noyau ***
begin
  ClearError; // pas d'erreur
  fWorkZone.Clear; // on nettoie
end;

(* ********************************************************************* *)

function TGVLogoKernel.Count: Integer;
// *** renvoie le nombre d'�l�ments du noyau ***
begin
  ClearError; // rien � signaler
  Result := fWorkZone.CountListP;
end;

(* ********************************************************************* *)

function TGVLogoKernel.CountItemsPck(const Name: string): Integer;
// *** nombre d'�l�ments d'un paquet ***
var
  S: string;
begin
  Result := -1; // suppose une erreur
  ClearError; // pas de message
  if IsPck(Name) then // est-ce un paquet ?
  begin
    Result := 0; // pas d'�l�ments
    for S in fWorkZone do // on balaie la zone de travail
      if (S <> EmptyStr) and IsInPck(S) // cherche propri�t� de paquet
        and AnsiSameText(BelongsTo(S), Name) then // la bonne ?
        Inc(Result); // si c'est bon, on incr�mente le r�sultat
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

constructor TGVLogoKernel.Create;
// *** cr�ation ***
begin
  inherited Create; // on h�rite
  fWorkZone := TGVPropList.Create; // on cr�e la liste de travail
  fLocalVars := TGVPropList.Create; // on cr�e la zone des variables locales
  fTempList := TGVListUtils.Create; // liste temporaire de travail
  fProtected := False; // pas de protection par d�faut
  OnChange := nil; // gestionnaires � nil
  OnKernelError := nil;
  Clear; // on nettoie
end;

(* ********************************************************************* *)

function TGVLogoKernel.CreatePck(const Name: string): Boolean;
// *** cr�ation d'un paquet ***
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if Exists(Name) then // le nom est-il pris ?
  begin
    if IsPck(Name) then // si oui, est-ce un paquet ?
      ErrorMessage(C_AlreadyPackage, Name) // si oui, erreur
    else
    begin
      if IsProtected(Name) then // �l�ment prot�g� ?
        ErrorMessage(C_Protected, Name) // erreur
      else
      begin
        if fWorkZone.RemoveListP(Name) then // on le d�truit
          Result := DProp(Name, CPackage, MF_True); // on le remplace
        if Result then
          Change; // changement notifi� si effectif
      end;
    end;
  end
  else // le nom est libre
  begin
    if IsValid(Name) then // nom correct ?
    begin
      Result := DProp(Name, CPackage, MF_True); // cr�ation du paquet
      if Result then
        Change; // changement notifi� si effectif
    end
    else
      ErrorMessage(C_BadName, Name); // nom incorrect
  end;
end;

(* ********************************************************************* *)

destructor TGVLogoKernel.Destroy;
// *** destruction ***
begin
  OnChange := nil; // gestionnaires � nil
  OnKernelError := nil;
  fWorkZone.Free; // on lib�re la zone de travail
  fLocalVars.Free; // idem pour les variables locales
  fTempList.Free; // lib�ration de la liste de travail
  inherited Destroy; // on h�rite
end;

(* ********************************************************************* *)

function TGVLogoKernel.DProp(const Name, Prop, Value: string): Boolean;
// *** d�finition d'une propri�t� ***
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if IsPrim(Name) then // est-ce une primitive ?
    ErrorMessage(C_PrimForbidden, Name) // on l'�pargne
  else
  begin
    Result := fWorkZone.UpDateListP(Name, Prop, Value); // mise � jour
    if not Result then
      ErrorMessage(C_BadProp, Name); // en cas d'erreur
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.EditToProc(Editor: TStrings; FromLine, ToLine: Integer;
  out Error: Integer): Boolean;
// *** �diteur vers proc�dure ***
// compile le contenu d'un �diteur � partir de la ligne indiqu�e (proc�dure apr�s proc�dure)
// Editor : liste des lignes de l'�diteur
// FromLine : premi�re ligne � analyser
// ToLine: derni�re ligne � analyser
// Error : premi�re ligne proc�dure fautive ou derni�re ligne analys�e
var
  Line, I: Integer;
  L1, L2: TGVList;
  St, Name, Def: string;
begin
  ClearError; // pas de message
  Error := 1; // ligne en cours pour erreur
  // on ajuste la recherche � la taille de l'�diteur
  if (ToLine > Editor.Count) or (ToLine = 0) then
    ToLine := Editor.Count;
  if FromLine < 1 then
    FromLine := 1;
  if (ToLine - FromLine) >= 0 then // s'il y a des lignes � analyser
  begin
    Line := FromLine-1; // d�part de l'analyse
    // tant qu'il y a des lignes et qu'il n'y a pas d'erreur
    while (Line < ToLine) and (fKernelResult = C_None) do
    begin
      L1 := TGVList.Create;
      try
        // r�cup�re la ligne en cours
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
            try // si erreur : mauvaise d�finition
              // on extrait le nom de la proc�dure (apr�s Pour !)
              if L1.Count > 1 then // un mot au moins suit ?
              begin
                Name := L1[1];
                St := CBeginList; // St est le caract�re [
                // *** construit la liste des param�tres si n�cessaire
                if L1.Count > 2 then
                  // on ajoute les param�tres (ni le Pour ni le nom)
                  for I := 2 to L1.Count - 1 do
                    if St = CBeginList then
                      St := St + L1[I]
                    else
                      St := St + CBlank + L1[I];
                // on stocke les param�tres
                Def := CBeginList + St + CEndList + CBlank;
                // *** on passe � la d�finition
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
                        // on cl�t la liste de d�finition
                        Def := Def + Trim(St) + CEndList + CEndList;
                        if AddProc(Name, Def) then // enregistre la d�finition
                        begin // l'enregistrement s'est bien d�roul�
                          fKernelResult := C_OKProc; // proc�dure enregistr�e
                          Change; // on signale le changement
                        end;
                      end
                      else
                        St := St + CBeginList + Trim(Editor[Line]) + CEndList +
                          CBlank; // on ajoute la ligne
                    end;
                  finally
                    L2.Free; // on lib�re la liste de travail
                  end;
                  if (Line > ToLine) and (fKernelResult <> C_OKProc) then
                  begin
                    Error := Line;
                    ErrorMessage(C_NotEnd, Name); // mot Fin non rencontr�
                  end;
                  Inc(Line);
                until (fKernelResult <> C_None); // on change de ligne
                if fKernelResult = C_OKProc then
                  // on annule le drapeau de mauvais enregistrement
                  ClearError;
              end
              else
                ErrorMessage(C_NoName, Editor[Line]); // pas de nom apr�s Pour
            except
              Error := Line;
              ErrorMessage(C_BadDef, Name); // mauvaise d�finition
            end;
          end
          else
          begin
            Error := Line;
            ErrorMessage(C_NotFor, EmptyStr); // c'est une erreur : pas de Pour
          end;
        end;
      finally
        L1.Free; // on lib�re la liste de travail
      end;
    end;
  end
  else
    ErrorMessage(C_EmptyEdit, EmptyStr); // pas de lignes � analyser
  Result := (fKernelResult = C_None); // OK si pas d'erreur...
end;

(* ********************************************************************* *)

function TGVLogoKernel.Exists(const Name: string): Boolean;
// *** v�rifie que le nom existe ***
begin
  ClearError; // rien � signaler
  Result := fWorkZone.IsListP(Name); // liste pr�sente ?
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
// *** r�sultat d'une op�ration ***
begin
  Result := fKernelResult; // enregistrement
  ClearError; // remise � z�ro
end;

(* ********************************************************************* *)

function TGVLogoKernel.GList(const Prop: string): string;
// *** liste des objets contenant la propri�t� ***
var
  S: string;
begin
  ClearError; // rien � signaler
  Result := CBeginList; // d�but de liste
  try
    for S in fWorkZone do // on balaie la zone de travail
      if (S <> EmptyStr) and // nom de la liste
        (Prop = CExtLP) or fWorkZone.IsProp(S, Prop) then
        // cherche la propri�t�
        Result := Result + S + CBlank; // si OK on ajoute le nom
  finally
    Result := TrimRight(Result) + CEndList; // on ferme la liste
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsEmptyPList(const Name: string): Boolean;
// *** la liste de propri�t� est-elle vide ? ***
begin
  ClearError; // rien � signaler
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
  ClearError; // rien � signaler
  if IsPck(Name) then // est-ce un paquet ?
    Result := fWorkZone.IsProp(Name, CBurried) // propri�t� enterr�e ?
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
  ClearError; // on annule une �ventuelle erreur
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsProc(const Name: string): Boolean;
// *** proc�dure ? ***
begin
  Result := IsObj(Name, CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsProtected(const Name: string): Boolean;
// *** �l�ment prot�g� ? ***
begin
  Result := False; // suppose que l'objet n'est pas prot�g�
  ClearError; // pas de message
  if Exists(Name) then // �l�ment existant ?
  begin
    if IsPck(Name) then // si oui, est-ce un paquet ?
      Result := True // un paquet est toujours prot�g�
    else // sinon chercher si pr�sent dans paquet
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
  else if Name <> EmptyStr then // pas de cha�ne vide !
  begin
    if Name[Length(Name)] = CAsk then // on accepte le "?" � la fin
    begin
      if Length(Name) <> 1 then // mais pas seulement !
        Result := IsValidIdent(Copy(Name, 1, Length(Name) - 1));
    end
    else if Name[1] = CDot then // on accepte le "." au d�but
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
// *** d�finition valide ? ***
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
      exit; // on sort de la proc�dure
    end;
    Lst2 := TGVList.Create; // premier �l�ment = param�tres
    try
      T := TStringList.Create; // liste des param�tres pour doublons
      try
        try
          Lst2.Text := Lst1.First;
          for I := 0 to Lst2.Count - 1 do // on rep�re les param�tres
            // s'il existe d�j� ou s'il est incorrect
            if (T.IndexOf(Lst2[I]) <> -1) or
              not IsValidParam(CBeginList + Lst2[I] + CEndList) then
            begin
              Result := False;
              ErrorMessage(C_BadParam, Lst2[I]); // mauvais param�tre
              break; // on sort de la boucle
            end
            else
            begin
              T.Add(Lst2[I]); // stocke le nouveau param�tre
              Result := True;
            end;
        except
          ErrorMessage(C_BadParam, St); // mauvais param�tres
        end;
      finally
        T.Free; // on lib�re la liste de travail
      end;
    finally
      Lst2.Free; // idem
    end;
    if Result then // si tout va bien : examinons la d�finition
    begin
      Lst2 := TGVList.Create; // on analyse les lignes
      try
        if (Lst1.Count > 1) then // on doit avoir une d�finition
        begin
          Lst3 := TGVList.Create; // on cr�e la liste des lignes
          try
            Lst3.Text := Lst1[1]; // on lui affecte la liste de d�finition
            for S in Lst3 do // on balaie les lignes
            begin
              Result := (fTempList.IsValid(S) or (S = EmptyStr));
              // liste valide ou vide
              if not Result then // sinon on sort
              begin
                ErrorMessage(C_BadDef, Lst1[1]); // mauvaise d�finition
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
        Lst2.Free; // on lib�re la liste
      end;
    end;
  finally
    Lst1.Free; // on lib�re la liste de travail
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsValidParam(const Lst: string): Boolean;
// *** les param�tres sont-ils valides ? ***
var
  L: TGVList;
  T: TStrings;
  S: string;
begin
  Result := (Lst = fTempList.EmptyList); // on accepte la liste vide
  ClearError; // rien � signaler
  if not Result then // autres cas
  begin
    L := TGVList.Create; // liste de travail
    try
      L.Text := Lst;
      T := TStringList.Create; // liste de cha�nes de travail
      try
        for S in L do // rep�re les param�tres
          if (T.IndexOf(S) <> -1) or // si existe d�j�
            not IsValidVar(S) then // ou incorrect
          begin
            Result := False; // ce n'est pas bon !
            ErrorMessage(C_BadParam, S); // mauvais param�tre
            break; // on sort de la boucle
          end
          else
          begin
            T.Add(S); // stocke le nouveau param�tre
            Result := True;
          end;
      finally
        T.Free; // on lib�re la liste de travail
      end;
    finally
      L.Free; // lib�ration de la liste de travail
    end;
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsValidVar(const Name: string): Boolean;
// *** nom de variable valide ? ***
begin
  Result := False; // suppose une erreur
  // cha�ne non vide et premier caract�re = ":" ?
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
        I := L.Count - 1; // nombre d'�l�ments
        while (I >= 0) do
        begin
          // si on parvient � empaqueter l'objet
          if ToPck(Name, L[I]) then
            Dec(I) // on passe au suivant
          else
            break; // sinon c'est le dernier
        end;
        Result := (fKernelResult = C_None); // pas d'erreur ?
      finally
        L.Free; // lib�ration de la liste de travail
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
// *** chargement g�n�rique ***
var
  L: TStringList;
  Lst: TGVList;
  StFile, S: string;
  I, J: Integer;
begin
  Result := False; // erreur par d�faut
  ClearError; // pas de message
  L := TStringList.Create; // on cr�e la liste provisoire
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
      // on v�rifie son contenu
      if (fKernelResult = C_None) then
      begin
        if L[0] = CHeader then // est-ce la bonne version ?
        begin
          L.Delete(0); // on �limine l'ent�te
          Lst := TGVList.Create;
          L.NameValueSeparator := CSep; // s�parateur entre noms et valeurs
          try
            if (fKernelResult = C_None) then // on continue si pas d'erreur
            begin
              for I := 0 to L.Count - 1 do // on balaie le fichier
              begin
                S := L.Names[I]; // on a le nom
                // est-ce que l'objet est prot�g� ?
                if IsProtected(S) then
                begin
                  ErrorMessage(C_Protected, S); // l'objet est prot�g�
                  exit;
                end;
                Lst.Text := L.Values[S]; // et les propri�t�s
                // est-ce le type cherch� ?
                if (Kind <> CExtLP) and (Lst.First <> Kind) then
                begin // le contenu est incorrect
                  ErrorMessage(C_BadContent, StFile);
                  exit;
                end;
                // est-ce que le nom est d�j� pris par un objet de nature diff�rente ?
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
            Lst.Free; // on lib�re la liste de travail
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
// *** compte des objets d'un type donn� ***
var
  S: string;
begin
  Result := 0; // pas d'objets
  ClearError; // rien � signaler
  for S in fWorkZone do // on balaie la zone de travail
    if (S <> EmptyStr) and // nom de la liste
      fWorkZone.IsProp(S, Kind) then // si c'est un objet du m�me type
      Inc(Result); // on incr�mente le r�sultat
end;

(* ********************************************************************* *)

function TGVLogoKernel.IsObj(const Name, Kind: string): Boolean;
// *** est-ce un objet du type donn� ? ***
begin
  ClearError; // rien � signaler
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
// *** supression d'un type d'objet donn� ***
var
  S: string;
begin
  Result := True; // suppose que tout est OK
  ClearError; // rien � signaler
  for S in fWorkZone do // on balaie la liste de travail
    // est-ce un objet de m�me type et non enterr� ?
    if IsObj(S, Kind) and not IsBurried(S) then
    begin
      Result := fWorkZone.RemoveListP(S); // on le d�truit
      if not Result then // une erreur ?
        break; // on quitte la boucle
    end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveObj(const Name, Kind: string): Boolean;
// *** supprime un objet selon son type ***
begin
  Result := False; // suppose une erreur
  ClearError; // rien � signaler
  if IsObj(Name, Kind) then // est-ce un objet du type demand� ?
  begin
    if IsProtected(Name) then // est-il prot�g� ?
      ErrorMessage(C_Protected, Name) // �l�ment prot�g�
    else
    begin
      Result := fWorkZone.RemoveProp(Name, Kind);
      // sinon, on supprime la propri�t�
      if Result then
      begin
        // s'il ne reste que la propri�t� de paquet => on supprime l'objet
        if (fWorkZone.CountProps(Name) = 1) and
          fWorkZone.IsProp(Name, CInPackage) then
          Result := fWorkZone.RemoveListP(Name);
        Change; // changement notifi� si effectif
      end;
    end;
  end
  else
    ErrorMessage(C_NotObject, Name); // ce n'est pas un objet adapt�
end;

(* ********************************************************************* *)

function TGVLogoKernel.LoadAll(const FileName: string): Boolean;
// *** chargement de l'espace ***
begin
  Result := Load(FileName, CExtLP);
end;

(* ********************************************************************* *)

function TGVLogoKernel.LoadProcs(const FileName: string): Boolean;
// *** chargement des proc�dures ***
begin
  try
    // on charge en �crasant si n�cessaire
    Result := Load(FileName, CProc);
  finally
    Change; // changement signal�
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.LoadVars(const FileName: string): Boolean;
// *** chargement de variables ***
begin
  try
    // on charge en �crasant si n�cessaire
    Result := Load(FileName, CVr);
  finally
    Change; // changement signal�
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
// *** nombre de param�tres de la primitive ***
begin
  ClearError; // pas de message
  Result := NumPrim(Name); // on cherche la primitive
  if (Result <> -1) then // trouv�e ?
    Result := GVPrimName[Result].NbParams // on renvoie le nombre de param�tres
  else
    ErrorMessage(C_NotPrim, Name); // ce n'est pas une primitive
end;

(* ********************************************************************* *)

function TGVLogoKernel.NumPrim(const Name: string): Integer;
// *** num�ro de primitive ***
var
  I: Integer;
begin
  ClearError; // pas de message
  Result := -1; // mauvais nombre
  for I := 1 to CPrimCount do // on balaie le tableau
  begin
    if AnsiSameText(GVPrimName[I].Name, Name) then // trouv�e ?
    begin
      Result := I; // on stocke le r�sultat
      break; // on sort de la boucle
    end;
  end;
  if Result = -1 then // non trouv�e
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
      begin // objet prot�g� ?
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
    Result := (fKernelResult = C_None); // r�sultat de sortie
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.UnPackObj(const Name: string): Boolean;
// *** d�paquette un objet ***
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
        // on enel�ve la propri�t�
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
  Result := False; // erreur par d�faut
  ClearError; // pas de message d'erreur
  if Exists(Name) then
    Result := fWorkZone.IsProp(Name, CInPackage) // recherche de la propri�t�
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
// *** l'objet est-il enterr� ? ***
begin
  Result := IsBurriedPck(BelongsTo(Name)); // recherche de la propri�t�
end;

(* ********************************************************************* *)

function TGVLogoKernel.BelongsTo(const Name: string): string;
// *** � quel paquet appartient l'objet ? ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
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
// *** d�compte du nombre de paquets ***
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
// *** paquet vers l'�diteur ***
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
      // on ajoute si proc�dure et propri�t� pr�sentes et correctes
        IsProc(S) and IsInPck(S) and
          AnsiSameText(fWorkZone.ValProp(S, CInPackage), Name) then
      begin
        Result := ProcToEdit(S, Lst); // on ajoute la proc�dure
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
        if (S <> EmptyStr) and // liste r�cup�r�e
        // si c'est la propri�t� paquet et si elle correspond � Name
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
// *** ligne de param�tres d'une proc�dure ***
var
  Lst: TGVList;
begin
  ClearError; // pas de message
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    Lst := TGVList.Create;
    try
      Lst.Text := RProp(Name, CProc); // cr�ation
      Result := Lst.First; // recherche des param�tres
    finally
      Lst.Free; // lib�re la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ParamsLine(const Name: string;
  out ParLine: string): Boolean;
// *** ligne de param�tres d'une proc�dure ***
var
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    Lst := TGVList.Create;
    try
      Lst.Text := RProp(Name, CProc); // cr�ation
      ParLine := Lst.First; // recherche des param�tres
      Result := True; // tout est OK
    finally
      Lst.Free; // lib�re la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ParamNum(const Name: string; Num: Integer): string;
// *** renvoie un param�tre d'une proc�dure ***
var
  S: string;
  Lst: TGVList;
begin
  ClearError; // pas de message
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    if (Num <= ParamsCount(Name)) and (Num >= 1) then
    begin
      S := ParamsLine(Name); // ligne des param�tres
      Lst := TGVList.Create; // liste de travail
      try
        Lst.Text := S;
        Result := Lst[Num-1]; // param�tre si possible
      finally
        Lst.Free; // lib�ration de la liste de travail
      end;
    end
    else
      ErrorMessage(C_BadParam, IntToStr(Num)); // mauvais param�tre
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ParamNum(const Name: string; Num: Integer;
  out ParNum: string): Boolean;
// *** renvoie un param�tre d'une proc�dure ***
var
  S: string;
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  ClearError; // pas de message
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    if (Num <= ParamsCount(Name)) and (Num >= 1) then
    begin
      S := ParamsLine(Name); // ligne des param�tres
      Lst := TGVList.Create; // liste de travail
      try
        Lst.Text := S;
        ParNum := Lst[Num-1]; // param�tre si possible
        Result := True;
      finally
        Lst.Free; // lib�ration de la liste de travail
      end;
    end
    else
      ErrorMessage(C_BadParam, IntToStr(Num)); // mauvais param�tre
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ParamsCount(const Name: string): Integer;
// *** nombre de param�tres d'une proc�dure ***
var
  Lst: TGVList;
begin
  Result := -1; // mauvais nombre
  ClearError; // pas de message
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := ParamsLine(Name);
      Result := Lst.Count; // nombre d'�l�ments
      if Lst.First = EmptyStr then
        Dec(Result);
    finally
      Lst.Free; // lib�ration de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.PListe(const Name: string): string;
// *** liste associ�e � une propri�t� ***
begin
  ClearError; // rien � signaler
  Result := fWorkZone.ValListP(Name); // renvoie la liste
  if Result = EmptyStr then // ne peut �tre une cha�ne vide
    ErrorMessage(C_NotLProp, Name); // pas une propri�t�
end;

(* ********************************************************************* *)

function TGVLogoKernel.PListe(const Name: string; out Value: string): Boolean;
// *** liste associ�e � une propri�t� ***
begin
  Result := False; // suppose une erreur
  ClearError; // rien � signaler
  Value := fWorkZone.ValListP(Name); // renvoie la liste
  if Value = EmptyStr then // ne peut �tre une cha�ne vide
    ErrorMessage(C_NotLProp, Name) // pas une propri�t�
  else
    Result := True; // tout est OK
end;

(* ********************************************************************* *)

function TGVLogoKernel.PrimByNum(const N: Integer): string;
// *** primitive par son num�ro ***
begin
  if (N > 0) and (N <= CPrimCount) then // dans les bornes ?
    Result := GVPrimName[N].Name // recherche du nom de la primitive
  else
    ErrorMessage(C_NotPrim, IntToStr(N)); // non !
end;

(* ********************************************************************* *)

function TGVLogoKernel.PrimByNum(const N: Integer;
  out PrimBNum: string): Boolean;
// *** primitive par son num�ro ***
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
// *** d�compte du nombre de primitives ***
begin
  Result := CPrimCount;
end;

(* ********************************************************************* *)

function TGVLogoKernel.PrimsToList: string;
// *** liste des primitives ***
var
  S: GVPrimRec;
begin
  Result := CBeginList; // d�but de liste
  try
    for S in GVPrimName do // on balaie le tableau
      Result := Result + S.Name + ' '; // on ajoute le nom trouv�
  finally
    Result := TrimRight(Result) + CEndList; // fin de liste
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcLine(const Name: string; Line: Integer): string;
// *** une ligne particuli�re d'une proc�dure ***
var
  S: string;
  Lst: TGVList;
begin
  ClearError; // rien � signaler
  Result := EmptyStr; // cha�ne vide par d�faut
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    S := ProcListDef(Name); // recherche de la d�finition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      if (Line > 0) and (Line <= Lst.Count) then
        Result := Lst[Line-1] // param�tre si possible
      else
        ErrorMessage(C_BadLine, IntToStr(Line)); // mauvaise ligne
    finally
      Lst.Free; // lib�ration de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcLine(const Name: string; Line: Integer;
  out PrLine: string): Boolean;
// *** une ligne particuli�re d'une proc�dure ***
var
  S: string;
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  ClearError; // rien � signaler
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    S := ProcListDef(Name); // recherche de la d�finition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      if (Line >= 0) and (Line < Lst.Count) then
      begin
        PrLine := Lst[Line]; // param�tre si possible
        Result := True; // tout est OK
      end
      else
        ErrorMessage(C_BadLine, IntToStr(Line)); // mauvaise ligne
    finally
      Lst.Free; // lib�ration de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcLinesCount(const Name: string): Integer;
// *** nombre de lignes d'une d�finition ***
var
  S: string;
  Lst: TGVList;
begin
  Result := -1; // mauvais nombre
  ClearError; // rien � signaler
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    S := ProcListDef(Name); // d�finition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      Result := Lst.Count; // nombre d'�l�ments
      if Lst.First = EmptyStr then
        Dec(Result);
    finally
      Lst.Free; // lib�ration de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcListDef(const Name: string): string;
// *** liste de d�finition d'une proc�dure ***
var
  S: string;
  Lst: TGVList;
begin
  ClearError; // rien � signaler
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    S := RProp(Name, CProc); // d�finition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      Result := Lst.Last; // recherche de la d�finition
    finally
      Lst.Free; // lib�ration de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcListDef(const Name: string;
  out PrListDef: string): Boolean;
// *** liste de d�finition d'une proc�dure ***
var
  S: string;
  Lst: TGVList;
begin
  Result := False;
  ClearError; // rien � signaler
  if IsProc(Name) then // est-ce une proc�dure ?
  begin
    S := RProp(Name, CProc); // d�finition
    Lst := TGVList.Create; // liste de travail
    try
      Lst.Text := S;
      PrListDef := Lst.Last; // recherche de la d�finition
      Result := True; // tout est OK
    finally
      Lst.Free; // lib�ration de la liste de travail
    end;
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcsCount: Integer;
// *** d�compte du nombre de proc�dures ***
begin
  Result := ObjCount(CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcsToList: string;
// *** liste des proc�dures ***
begin
  Result := GList(CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcToEdit(const Name: string; Lst: TStrings): Boolean;
// *** proc�dure vers �diteur ***
var
  I, J: Integer;
  S: string;
begin
  Result := False; // suppose une erreur
  ClearError; // rien � signaler
  if IsProc(Name) then // la proc�dure existe-t-elle ?
  begin
    S := EmptyStr; // cha�ne vide
    J := ParamsCount(Name); // nombre de param�tres
    if (J <> 0) then // si au moins un param�tre
      for I := 1 to J do // les recherche
        S := S + CBlank + ParamNum(Name, I); // param�tres
    Lst.Add(P_For + CBlank + Name + S); // ent�te
    J := ProcLinesCount(Name); // nombre de lignes
    if (J <> 0) then // s'il y a au moins une ligne
      for I := 1 to J do // les recherche et les ajoute
        Lst.Add(CBlank + CBlank + fTempList.ListToStr(ProcLine(Name, I)));
    Lst.Add(P_End); // fin de la proc�dure
    Lst.Add(EmptyStr); // espace
    Result := True; // tout est OK
  end
  else
    ErrorMessage(C_NotProc, Name); // ce n'est pas une proc�dure
end;

(* ********************************************************************* *)

function TGVLogoKernel.ProcsToEdit(const LstP: string; Lst: TStrings): Boolean;
// *** envoie une liste de proc�dures vers un �diteur ***
var
  S: string;
  L: TGVList;
begin
  Result := False; // suppose une erreur
  L := TGVList.Create;
  try
    if fTempList.IsValid(LstP) then // la liste est-elle valide ?
    begin
      L.Text := LstP; // si oui on l'affecte � celle de travail
      for S in L do // on balaie les valeurs
      begin
        Result := ProcToEdit(S, Lst); // on envoie une proc�dure
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
// *** d�truit toutes les proc�dures ***
begin
  Result := RemoveAllObj(CProc);
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveAllVars: Boolean;
// *** d�truit toutes les variables ***
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
  ClearError; // rien � signaler
  if IsPck(Name) then // est-ce un paquet ?
  begin
    Result := True; // tout va bien
    for S in fWorkZone do // on balaie la zone de travail
    begin
      if (S <> EmptyStr) and
      // cherche la propri�t� paquet
        AnsiSameText(BelongsTo(S), Name) then
      begin
        Result := fWorkZone.RemoveProp(S, CInPackage);
        // on supprime la propri�t�
        if not Result then
          break; // on sort de la boucle si erreur
      end;
    end;
    if Result then // si tout est Ok
    begin
      Result := fWorkZone.RemoveListP(Name); // on supprime le paquet
      if Result then
        Change; // changement notifi� si effectif
    end;
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.RemoveProc(const Name: string): Boolean;
// *** suppression d'une proc�dure ***
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
  ClearError; // rien � signaler
  L := TGVList.Create; // liste des objets � supprimer
  try
    L.Text := Lst; // on construit la liste des objets � supprimer
    I := L.Count - 1; // on compte les �l�ments
    if I = -1 then
      ErrorMessage(C_EmptyList, Lst); // signalement d'une liste vide
    while (I >= 0) do
    begin
      // on v�rifie l'existence de l'objet
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
// destruction d'une liste de proc�dures
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
// *** valeur d'une propri�t� ***
begin
  ClearError; // rien � signaler
  Result := EmptyStr; // cha�ne vide par d�faut
  if Exists(Name) then // nom existe ?
  begin
    if fWorkZone.IsProp(Name, Prop) then
      Result := fWorkZone.ValProp(Name, Prop) // renvoie la valeur
    else
      ErrorMessage(C_NotProp, Prop); // propri�t� inconnue
  end
  else
    ErrorMessage(C_NotLProp, Name); // liste inconnue
end;

(* ********************************************************************* *)

function TGVLogoKernel.RProp(const Name, Prop: string;
  out Value: string): Boolean;
// *** valeur d'une propri�t� ***
begin
  Result := False; // suppose une erreur
  ClearError; // rien � signaler
  if Exists(Name) then // nom existe ?
  begin
    if fWorkZone.IsProp(Name, Prop) then
    begin
      Value := fWorkZone.ValProp(Name, Prop); // renvoie la valeur
      Result := True;
    end
    else
      ErrorMessage(C_NotProp, Prop); // propri�t� inconnue
  end
  else
    ErrorMessage(C_NotLProp, Name); // liste inconnue
end;

(* ********************************************************************* *)

function TGVLogoKernel.Save(const FileName, Lst, Kind: string): Boolean;
// *** sauvegarde g�n�rique ***
var
  LstPL: TGVPropList;
  L1, L2: TGVList;
  I, J: Integer;
begin
  Result := False; // on suppose une erreur
  ClearError; // rien � signaler
  LstPL := TGVPropList.Create; // liste de propri�t�s provisoire cr��e
  L1 := TGVList.Create; // liste des objets � sauver
  try
    L1.Text := Lst; // on construit la liste des objets � sauver
    I := L1.Count - 1; // on compte les �l�ments
    if I = -1 then
      ErrorMessage(C_EmptyList, Lst); // signalement d'une liste vide
    L2 := TGVList.Create;
    try
      while (I >= 0) do // on transf�re uniquement les objets existants
      begin
        if ((Kind = CExtLP) and Exists(L1[I])) or fWorkZone.IsProp(L1[I], Kind)
        then // si trouv� on m�morise
        begin
          L2.Text := fWorkZone.ValListP(L1[I]); // on recherche les propri�t�s
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
    LstPL.Free; // lib�rer la liste provisoire
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
// *** sauve toutes les proc�dures ***
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
  ClearError; // rien � signaler
  if IsPck(Name) then // est-ce un paquet ?
  begin
    St := CBeginList; // d�but de liste
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
// *** sauve une liste de proc�dures ***
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
// *** �met une erreur si l'objet n'existe pas
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
  ClearError; // rien � signaler
  Result := CBeginList; // on ouvre la liste
  try
    for S in fWorkZone do
      if (S <> EmptyStr) then
        Result := Result + S + CBlank; // on ajoute le nom au r�sultat
  finally
    Result := TrimRight(Result) + CEndList; // on ferme la liste
  end;
end;

(* ********************************************************************* *)

function TGVLogoKernel.ToPck(const Name, Obj: string): Boolean;
// *** ins�re un objet dans un paquet ***
begin
  Result := False; // on suppose une erreur
  ClearError; // rien � signaler
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if Exists(Obj) then // l'objet existe-t-il ?
    begin
      if IsPck(Obj) then // est-ce un paquet ?
        ErrorMessage(C_PackageForbidden, Name) // ne peut �tre empaquet�
      else
      begin
        // une primitive non plus 
        if IsPrim(Obj) then
          ErrorMessage(C_PrimForbidden, Obj)
        else
        begin
          if IsProtected(Obj) then // objet prot�g� ?
            ErrorMessage(C_Protected, Obj)
          else
          begin
            Result := DProp(Obj, CInPackage, Name); // mise � jour
            Change; // changement signal�
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
// d�terre un paquet
begin
  Result := False; // on suppose une erreur
  ClearError; // rien � signaler
  if IsPck(Name) then // est-ce un paquet ?
  begin
    if IsBurriedPck(Name) then // enterr� ?
    begin
      Result := fWorkZone.RemoveProp(Name, CBurried); // on le d�terre
      if Result then
        Change; // changement notifi�
    end
    else
      ErrorMessage(C_NotBurried, Name); // paquet non enterr�
  end
  else
    ErrorMessage(C_NotPackage, Name); // ce n'est pas un paquet
end;

(* ********************************************************************* *)

function TGVLogoKernel.ValVar(const Name: string; out Value: string): Boolean;
// *** valeur d'une variable ***
begin
  Result := False; // suppose une erreur
  ClearError; // rien � signaler
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
  ClearError; // rien � signaler
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
  ClearError; // rien � signaler
  if IsLocVar(Name) then // est-ce une variable locale ?
    Result := fTempList.ListToStr(fLocalVars.ValProp(Name, CVr)) // on rend sa valeur
  else
    ErrorMessage(C_NotVar, Name); // ce n'est pas une variable
end;

(* ********************************************************************* *)

function TGVLogoKernel.ValVar(const Name: string): string;
// *** valeur d'une variable ***
begin
  ClearError; // rien � signaler
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
