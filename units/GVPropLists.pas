{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Listes de propriétés                    |
  |                  Unité : GVPropLists.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVPROPLISTS - part of GVLOGO
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

unit GVPropLists;
// L'unité GVLISTS regroupe les classes chargées de traiter les listes
// de propriétés du projet GVLOGO.
//
// Une liste de propriétés associe des valeurs à des caractéristiques
// choisies pour un objet.
// Ainsi, un chien pourra être défini par les propriétés : race, âge, sexe.
// Chacune de ces propriétés aura une valeur particulière.
//
// Les listes de propriétés sont elles-mêmes à la base du noyau de
// l'interpréteur GVLOGO.
//

interface

uses
  Classes,
  SysUtils,
  GVConsts, // pour les constantes
  GVWords, // pour les mots
  GVLists, // pour les listes
  GVErrConsts, // constantes d'erreurs
  GVErrors; // pour les erreurs

type
  // *** classe pour énumération ***
  TGVPropListEnumerator = class(TObject)
  private
    fLst: TStringList;
    fIndex: Integer;
  protected
    function GetCurrent: string; virtual; // élément en cours
  public
    constructor Create(const Value: TStrings); // création
    destructor Destroy; override; // destruction
    function MoveNext: Boolean; // vers le suivant
    property Current: string read GetCurrent; // valeur de l'élément courant
  end;

  // *** classe des listes de propriétés ***
  TGVPropList = class(TObject)
  strict private
    fError: TGVErrors; // enregistrement d'une erreur
    fNames: TStringList; // listes
    fOnchange: TNotifyEvent; // notification de changement
    function GetLPByNum(N: Integer): string; // liste par numéro
    function GetLPByName(const Name: string): string; // renvoie liste par nom
    procedure SetLPByName(const Name, Value: string); // fixe liste par nom
  protected
    procedure Change; // changement
  public
    // constructeur de la classe
    constructor Create;
    // destructeur de la classe
    destructor Destroy; override; // destructeur
    // énumération
    function GetEnumerator: TGVPropListEnumerator;
    // nettoie les listes de propriétés
    procedure Clear;
    // *** listes de propriétés ***
    // renvoie la liste des listes de propriétés
    function ListP: string;
    // la liste existe-t-elle ?
    function IsListP(const Name: string): Boolean;
    // renvoie le numéro d'une liste de propriétés
    function NumListP(const Name: string): Integer;
    // crée ou met à jour la liste de propriétés
    function UpDateListP(const Name, Prop, Value: string): Boolean;
    // renvoie la valeur d'une liste
    function ValListP(const Name: string): string;
    // destruction d'une liste de propriétés
    function RemoveListP(const Name: string): Boolean;
    // valeur d'une liste de propriétés par numéro
    function ValNumListP(N: Integer; out Name, Value: string): Boolean;
    // la propriété N existe-t-elle?
    function IsListPByNum(N: Integer): Boolean;
    // renvoie le nombre de listes de propriétés
    function CountListP: Integer;
    // chargement des listes
    procedure LoadFromFile(const FileName: string);
    // sauvegarde des listes
    procedure SaveToFile(const FileName: string);
    // *** propriétés
    // la propriété existe-t-elle ?
    function IsProp(const Name, Prop: string): Boolean;
    // renvoie le numéro d'une propriété
    function NumProp(const Name, Prop: string): Integer;
    // valeur d'une propriété
    function ValProp(const Name, Prop: string): string;
    // destruction d'une propriété
    function RemoveProp(const Name, Prop: string): Boolean;
    // renvoie le nombre de propriétés attachées à une liste
    function CountProps(const Name: string): Integer;
    // valeur d'une propriété par numéro
    function ValNumProp(const Name: string; N: Integer; out Prop: string)
      : Boolean; overload;
    function ValNumProp(const Name: string; N: Integer): string;
    // liste des propriétés d'une liste
    function ListOfProps(const Name: string): string;
    // nom d'une propriété par numéro
    function NameOfProp(const Name: string; N: Integer): string; overload;
    // changement dans la liste de propriétés
    property OnChange: TNotifyEvent read fOnchange write fOnchange;
    // liste de propriétés par numéro
    property ListPByNum[N: Integer]: string read GetLPByNum; default;
    // liste de propriétés par nom
    property LPByName[const Name: string]: string read GetLPByName
       write SetLPByName;
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
  end;

implementation

uses
  StrUtils,
  lazutf8; // traitement des chaînes UTF8

{ TGVPropListEnumerator }

function TGVPropListEnumerator.GetCurrent: string;
// *** retourne l'élément courant ***
begin
  Result := fLst.Names[fIndex];
end;

constructor TGVPropListEnumerator.Create(const Value: TStrings);
// *** création de l'énumérateur ***
begin
  fIndex := -1; // on pointe avant le premier élément
  fLst := TStringList.Create; // liste interne
  fLst.NameValueSeparator := CSep; // définit le caractère séparateur
  fLst.AddStrings(Value); // on intègre les valeurs à énumérer
end;

destructor TGVPropListEnumerator.Destroy;
// *** destruction de l'énumérateur ***
begin
  fLst.Free; // liste interne libérée
  inherited Destroy; // on hérite
end;

function TGVPropListEnumerator.MoveNext: Boolean;
// *** passe à l'élément suivant ***
begin
  Result := fIndex < (fLst.Count - 1); // on vérifie la borne supérieure
  if Result then // si OK
    Inc(fIndex); // on passe à l'élément suivant
end;

{ ========================================================== }

{ TGVPropList }

function TGVPropList.GetLPByNum(N: Integer): string;
// *** renvoie la liste complète ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if (N > 0) and (N <= fNames.Count) then // intervalle possible ?
    Result := fNames[N - 1]
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownNumListP, IntToStr(N));
end;

procedure TGVPropList.Change;
// *** changement dans la liste ***
begin
  if Assigned(fOnchange) then // gestionnaire actif ?
    fOnchange(Self); // on l'exécute
end;

procedure TGVPropList.Clear;
// *** nettoie les listes de propriétés ***
begin
  fNames.Clear; // pas de noms
  Error.Clear; // pas d'erreur
  Change; // changement notifié
end;

function TGVPropList.CountListP: Integer;
// *** nombre de listes de propriétés ***
begin
  Result := fNames.Count; // = nombre de noms enregistrés
end;

function TGVPropList.CountProps(const Name: string): Integer;
// *** nombre de propriétés attachées à une liste ***
var
  LL: TGVList;
begin
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
    LL := TGVList.Create; // création de la liste provisoire
    try
      LL.Text := fNames.Values[Name]; // analyse de la liste
      Result := LL.Count div 2; // compte les éléments (par paires)
    finally
      LL.Free; // libération de la liste de travail
    end;
  end
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

constructor TGVPropList.Create;
// *** constructeur de la classe ***
begin
  inherited Create; // on hérite
  fNames := TStringList.Create; // on crée la liste interne
  fNames.NameValueSeparator := CSep; // on définit le caractère séparateur
  fOnchange := nil; // on met à nil le gestionnaire de changement
  fError := TGVErrors.Create; // création du gestionnaire d'erreurs
end;

destructor TGVPropList.Destroy;
// *** destructeur de la classe ***
begin
  fOnchange := nil; // gestionnaire d'événements annulé
  fNames.Free; // on libère les noms
  fError.Free; // on libère le gestionnaire d'erreurs
  inherited Destroy; // on hérite
end;

function TGVPropList.GetEnumerator: TGVPropListEnumerator;
// *** mise en place de l'énumération ***
begin
  Result := TGVPropListEnumerator.Create(fNames);
end;

function TGVPropList.GetLPByName(const Name: string): string;
// *** liste de propriétés par nom ***
var
  Li: Integer;
begin
  Result := EmptyStr; // chaîne vide par défaut
  Li := fNames.IndexOfName(Name); // recherche de l'existence
  if Li <> -1 then // si trouvée
    Result := fNames[Li] // on renvoie sa valeur
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

procedure TGVPropList.SetLPByName(const Name, Value: string);
// *** définit une liste de propriétés directement ***
begin
  fNames.Values[Name] := Value;
end;

function TGVPropList.IsListPByNum(N: Integer): Boolean;
// *** la propriété N existe-t-elle ? ***
begin
  Result := (N > 0) and (N <= fNames.Count); // dans les bornes ?
end;

function TGVPropList.IsListP(const Name: string): Boolean;
// *** la liste existe-t-elle ? ***
begin
  Result := (fNames.IndexOfName(Name) <> -1);
end;

function TGVPropList.IsProp(const Name, Prop: string): Boolean;
// *** la propriété existe-t-elle ? ***
var
  LL: TGVList;
  Li: Integer;
begin
  Result := False; // on suppose l'absence
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
    LL := TGVList.Create;
    try
      LL.Text := ValListP(Name); // recherche de la valeur de la liste
      for Li := 1 to LL.Count do // analyse de la liste
      begin
        // cherche les éléments et teste la propriété
        Result := Odd(Li) and (UTF8CompareText(LL[Li - 1], Prop) = 0);
        if Result then
          Break; // on a trouvé, donc on sort
      end;
    finally
      LL.Free; // libération de la liste de travail
    end;
  end
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.ListOfProps(const Name: string): string;
// *** liste des propriétés d'une liste ***
var
  Li: Integer;
  LL: TGVList;
begin
  Result := CBeginList; // début de la liste
  try
    if IsListP(Name) then
    begin
      LL := TGVList.Create; // création de la liste de travail
      try
        LL.Text := ValListP(Name); // récupère la liste
        for Li := 1 to LL.Count do // on balaie les propriétés
          if Odd(Li) then // propriétés par paires
            Result := Result + LL[Li - 1] + CBlank; // on ajoute le nom
      finally
        LL.Free; // libération de la liste de travail
      end;
    end
    else
      // [### Erreur: liste de propriétés inexistante ###]
      Error.SetError(CE_UnknownListP, Name);
  finally
    Result := TrimRight(Result) + CEndList; // fin de la liste
  end;
end;

function TGVPropList.ListP: string;
// *** renvoie la liste des listes de propriétés ***
var
  LSt: string;
begin
  Result := CBeginList;
  try
    for LSt in fNames do // on balaie la liste
      if (Result <> CBeginList) then
        Result := Result + CBlank + LSt // pour construire la chaîne
      else
        Result := Result + LSt;
    // on remplace le caractère séparateur
    Result := AnsiReplaceText(Result, CSep, CBlank);
  finally
    Result := Result + CEndList;
  end;
end;

procedure TGVPropList.LoadFromFile(const FileName: string);
// ***  chargement depuis un fichier ***
var
  LLst: TStringList;
  LL: TGVList;
  LSt, GVName: string;
  Li, Lj: Integer;
begin
  LLst := TStringList.Create; // liste de travail
  LLst.NameValueSeparator := CSep; // séparateur pour les propriétés
  try
    // change l'extension si nécessaire
    if PosEx(CDot, FileName) = 0 then // si aucune extension
      LSt := FileName + CExtPl // on en ajoute une (.GPL)
    else
      LSt := FileName;
    LLst.LoadFromFile(LSt); // charge le fichier
    if LLst[0] <> CHeader then
    begin
      // [### Erreur: mauvais format de fichier ###]
      Error.SetError(CE_BadFileFormat, Lst, 1);
      Exit; // inutile d'aller plus loin
    end;
    LLst.Delete(0); // on élimine l'entête
    LL := TGVList.Create; // liste intermédiaire créée
    try
      for Li := 0 to LLst.Count - 1 do // on la balaie
      begin
        if Error.OK then // on met à jour si pas d'erreur
        begin
          GVName := LLst.Names[Li]; // on a le nom
          LL.Text := LLst.Values[GVName]; // et les propriétés
          for Lj := 1 to LL.Count do
            if Odd(Lj) then
              UpDateListP(GVName, LL[Lj - 1], LL[Lj]); // on met à jour
        end;
      end;
    finally
      LL.Free; // on libère la liste intermédiaire
    end;
  finally
    LLst.Free; // on libère la liste de travail
  end;
end;

function TGVPropList.NameOfProp(const Name: string; N: Integer): string;
// *** nom d'une propriété par numéro ***
var
  LL: TGVList;
begin
  Result := EmptyStr; // chaîne vide par défaut
  if IsListP(Name) then // est-ce une liste de propriétés ?
  begin
    LL := TGVList.Create;
    try
      if (N > 0) and (N <= CountProps(Name)) then // dans les bornes ?
      begin
        LL.Text := ValListP(Name); // liste de travail
        Result := LL[(N - 1) * 2]; // propriétés par paires
      end
      else
       // [### Erreur: propriété inexistante ###]
       Error.SetError(CE_UnknownNumProp, IntToStr(N));
    finally
      LL.Free; // on libère la liste
    end;
  end
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.NumListP(const Name: string): Integer;
// *** recherche d'une liste de propriétés ***
begin
  Result := fNames.IndexOfName(Name) + 1;
  if Result = 0 then
    Result := -1; // non trouvée
end;

function TGVPropList.NumProp(const Name, Prop: string): Integer;
// *** recherche du numéro d'une propriété ***
var
  LL: TGVList;
  Li: Integer;
begin
  Result := -1; // on suppose une erreur
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
    LL := TGVList.Create;
    try
      LL.Text := ValListP(Name); // recherche de la valeur de la liste
      for Li := 1 to LL.Count do // analyse de la liste
        // cherche les éléments et teste la propriété
        if Odd(Li) and (UTF8CompareText(LL[Li - 1], Prop) = 0) then
        begin
          Result := ((Li - 1) div 2) + 1; // toujours par paires
          Break; // on a trouvé, donc on sort
        end;
    finally
      LL.Free; // libération de la liste de travail
    end;
    if Result = -1 then // propriété non trouvée
      // [### Erreur: propriété inexistante ###]
      Error.SetError(CE_UnknownProp, Prop);
  end
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.RemoveListP(const Name: string): Boolean;
// *** détruit la liste de propriétés ***
var
  Li: Integer;
begin
  Result := False; // suppose une erreur
  Li := fNames.IndexOfName(Name); // on cherche la liste
  if (Li <> -1) then
  begin
    fNames.Delete(Li); // on la détruit
    Result := True;
    Change; // on notifie les changements
  end
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.RemoveProp(const Name, Prop: string): Boolean;
// *** détruit une propriété ***
var
  LL: TGVList;
  LUtil: TGVListUtils;
begin
  Result := False; // suppose une erreur
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
    LUtil := TGVListUtils.Create;
    try
      LL := TGVList.Create; // liste temporaire
      try
        LL.Text := ValListP(Name); // recherche de la valeur de la liste
        // on retire deux éléments si possible
        if IsProp(Name, Prop) then
        begin
          fNames.Values[Name] := LL.TwoDelete(NumProp(Name, Prop) * 2 - 1);
          if ValListP(Name) = LUtil.EmptyList then
            // on détruit la liste si vide
            Result := RemoveListP(Name)
          else
            Result := True;
          Change; // on notifie les changements
        end
        else
          // [### Erreur: propriété inexistante ###]
          Error.SetError(CE_UnknownProp, Prop);
      finally
        LL.Free; // on libère la liste de travail
      end;
    finally
      LUtil.Free;
    end;
  end
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

procedure TGVPropList.SaveToFile(const FileName: string);
// *** sauvegarde des listes de propriétés ***
var
  LL: TStringList;
  LSt: string;
begin
  LL := TStringList.Create; // liste de travail
  try
    // change l'extension si nécessaire
    if PosEx(CDot, FileName) = 0 then
      LSt := FileName + CExtPl
    else
      LSt := FileName;
    LL.Add(CHeader); // ajoute l'entête
    LL.AddStrings(fNames); // ajoute les listes
    try
      LL.SaveToFile(LSt); // sauve la liste
    except
      // [### Erreur: mauvaise sauvegarde ###]
      Error.SetError(CE_BadSave, LSt);
    end;
  finally
    LL.Free; // libère la liste de travail
  end;
end;

function TGVPropList.UpDateListP(const Name, Prop, Value: string): Boolean;
// *** crée ou met à jour la liste de propriétés ***
var
  LL: TGVList;
  Li: Integer;
  LWord: TGVWord;
  LUtil: TGVListUtils;
  LS: string;
begin
  LUtil := TGVListUtils.Create;
  try
    LWord := TGVWord.Create;
    try
      // test des valeurs
      LWord.Text := Name;
      Result := (Name <> EmptyStr) and LWord.IsValidIdent;
      if not Result then // nom incorrect ?
      begin
        // [### Erreur: mot incorrect ###]
        Error.SetError(CE_BadWord, Name);
        Exit; // on sort
      end;
      LWord.Text := Prop;
      Result := (Prop <> EmptyStr) and LWord.IsValid;
      if not Result then // nom de propriété incorrect ?
      begin
        // [### Erreur: mot incorrect ###]
        Error.SetError(CE_BadWord, Prop);
        Exit; // on sort
      end;
      Result := LUtil.IsValidValue(Value);
      if not Result then  // valeur de propriété incorrecte ?
      begin
        // [### Erreur: ni une liste ni un mot ###]
        Error.SetError(CE_UnknownListWord, Value);
        Exit; // on sort
      end
      else // si tout est Ok
      begin
        LS := CBeginList + Value + CEndList; // valeur en liste
        if IsListP(Name) then // la liste existe-t-elle déjà ?
        begin // oui
          LL := TGVList.Create;
          try
            LL.Text := ValListP(Name); // cherche les propriétés
            if IsProp(Name, Prop) then
            begin
              Li := NumProp(Name, Prop); // numéro de la propriété
              // on remplace
              fNames.Values[Name] := LL.ReplaceItem(2 * Li, LS);
            end
            else
              fNames.Values[Name] := LL.TwoAdd(Prop, LS); // on ajoute
            Change; // notifie le changement
          finally
            LL.Free; // on libère la liste de travail
          end;
        end
        else // création de la nouvelle liste
        begin
          fNames.Add(Name + fNames.NameValueSeparator + CBeginList + Prop +
            CBlank + LS + CEndList); // on crée le nom
          Change; // notifie le changement
        end;
      end;
    finally
      LWord.Free; // libération du mot de travail
    end;
  finally
    LUtil.Free; // libération de la chaîne utilitaire
  end;
end;

function TGVPropList.ValListP(const Name: string): string;
// *** valeur d'une liste de propriétés ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if IsListP(Name) then // la liste existe-t-elle ?
    Result := fNames.Values[Name] // retourne les propriétés
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.ValNumListP(N: Integer; out Name, Value: string): Boolean;
// *** valeur d'une liste de propriétés par numéro ***
begin
  Result := False; // suppose une erreur
  if (N > 0) and (N <= fNames.Count) then // dans les bornes ?
  begin
    Name := fNames.Names[N-1]; // d'abord le nom
    Value := fNames.ValueFromIndex[N-1]; // puis les propriétés
    Result := True; // tout va bien
  end
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownNumListP, IntToStr(N));
end;

function TGVPropList.ValNumProp(const Name: string; N: Integer): string;
// *** valeur d'une propriété par numéro *** ###
var
  LL: TGVList;
  LU: TGVListUtils;
begin
  Result := EmptyStr; // chaîne vide par défaut
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
    LL := TGVList.Create;
    try
      LU := TGVListUtils.Create; // utilitaire
      try
      if (N > 0) and (N <= CountProps(Name)) then // dans les bornes ?
      begin
        LL.Text := ValListP(Name); // liste de travail
        // propriétés par paires, valeur sans les crochets
        Result := LU.ListToStr(LL[(N - 1) * 2]);
      end
      else
        // [### Erreur: propriété inexistante ###]
        Error.SetError(CE_UnknownNumProp, IntToStr(N));
      finally
        LU.Free; // utilitaire libéré
      end;
    finally
      LL.Free; // on libère la liste
    end;
  end
  else
    // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.ValNumProp(const Name: string; N: Integer;
  out Prop: string): Boolean;
// *** valeur d'une propriété par numéro ***
var
  LL: TGVList;
  LU: TGVListUtils;
begin
  Result := False; // suppose une erreur
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
    LL := TGVList.Create;
    try
      LU := TGVListUtils.Create; // utilitaire
      try
        if (N > 0) and (N <= CountProps(Name)) then // dans les bornes ?
        begin
          LL.Text := ValListP(Name); // liste de travail
          Prop := LU.ListToStr(LL[(N - 1) * 2]); // propriétés par paires
          Result := True; // tout est Ok
        end
        else
         // [### Erreur: propriété inexistante ###]
         Error.SetError(CE_UnknownNumProp, IntToStr(N));
      finally
        LU.Free; // utilitaire libéré
      end;
    finally
      LL.Free; // on libère la liste
    end;
  end
  else
   // [### Erreur: liste de propriétés inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.ValProp(const Name, Prop: string): string;
// *** valeur d'une propriété ***
var
  LL: TGVList;
  Li: Integer;
  LU: TGVListUtils;
begin
  Result := EmptyStr; // chaîne vide par défaut
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
  LL := TGVList.Create;
  try
    if IsProp(Name, Prop) then
    begin
      LL.Text := ValListP(Name); // liste de travail
      LU := TGVListUtils.Create; // création de l'utilitaire
      try
      for Li := 1 to LL.Count do // on examine la liste
        // si c'est une propriété et qu'elle correspond à celle cherchée
        if Odd(Li) and (UTF8CompareText(LL[Li - 1], Prop) = 0) then
        begin
          Result := Lu.ListToStr(LL[Li]); // élément trouvé
          Break; // on sort de la boucle
        end;
      finally
        LU.Free; // libération de l'utilitaire
      end;
    end
    else
      // [### Erreur: propriété inexistante ###]
      Error.SetError(CE_UnknownProp, Prop);
  finally
    LL.Free; // on libère la liste
  end;
  end
  else
    // [### Erreur: propriété inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

end.
