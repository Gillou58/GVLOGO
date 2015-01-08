{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Listes de propri�t�s                    |
  |                  Unit� : GVPropLists.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - premi�re version op�rationnelle

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

{$I GVDefines.inc} // fichier des d�finitions pr�alables

unit GVPropLists;
// L'unit� GVLISTS regroupe les classes charg�es de traiter les listes
// de propri�t�s du projet GVLOGO.
//
// Une liste de propri�t�s associe des valeurs � des caract�ristiques
// choisies pour un objet.
// Ainsi, un chien pourra �tre d�fini par les propri�t�s : race, �ge, sexe.
// Chacune de ces propri�t�s aura une valeur particuli�re.
//
// Les listes de propri�t�s sont elles-m�mes � la base du noyau de
// l'interpr�teur GVLOGO.
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
  // *** classe pour �num�ration ***
  TGVPropListEnumerator = class(TObject)
  private
    fLst: TStringList;
    fIndex: Integer;
  protected
    function GetCurrent: string; virtual; // �l�ment en cours
  public
    constructor Create(const Value: TStrings); // cr�ation
    destructor Destroy; override; // destruction
    function MoveNext: Boolean; // vers le suivant
    property Current: string read GetCurrent; // valeur de l'�l�ment courant
  end;

  // *** classe des listes de propri�t�s ***
  TGVPropList = class(TObject)
  strict private
    fError: TGVErrors; // enregistrement d'une erreur
    fNames: TStringList; // listes
    fOnchange: TNotifyEvent; // notification de changement
    function GetLPByNum(N: Integer): string; // liste par num�ro
    function GetLPByName(const Name: string): string; // renvoie liste par nom
    procedure SetLPByName(const Name, Value: string); // fixe liste par nom
  protected
    procedure Change; // changement
  public
    // constructeur de la classe
    constructor Create;
    // destructeur de la classe
    destructor Destroy; override; // destructeur
    // �num�ration
    function GetEnumerator: TGVPropListEnumerator;
    // nettoie les listes de propri�t�s
    procedure Clear;
    // *** listes de propri�t�s ***
    // renvoie la liste des listes de propri�t�s
    function ListP: string;
    // la liste existe-t-elle ?
    function IsListP(const Name: string): Boolean;
    // renvoie le num�ro d'une liste de propri�t�s
    function NumListP(const Name: string): Integer;
    // cr�e ou met � jour la liste de propri�t�s
    function UpDateListP(const Name, Prop, Value: string): Boolean;
    // renvoie la valeur d'une liste
    function ValListP(const Name: string): string;
    // destruction d'une liste de propri�t�s
    function RemoveListP(const Name: string): Boolean;
    // valeur d'une liste de propri�t�s par num�ro
    function ValNumListP(N: Integer; out Name, Value: string): Boolean;
    // la propri�t� N existe-t-elle?
    function IsListPByNum(N: Integer): Boolean;
    // renvoie le nombre de listes de propri�t�s
    function CountListP: Integer;
    // chargement des listes
    procedure LoadFromFile(const FileName: string);
    // sauvegarde des listes
    procedure SaveToFile(const FileName: string);
    // *** propri�t�s
    // la propri�t� existe-t-elle ?
    function IsProp(const Name, Prop: string): Boolean;
    // renvoie le num�ro d'une propri�t�
    function NumProp(const Name, Prop: string): Integer;
    // valeur d'une propri�t�
    function ValProp(const Name, Prop: string): string;
    // destruction d'une propri�t�
    function RemoveProp(const Name, Prop: string): Boolean;
    // renvoie le nombre de propri�t�s attach�es � une liste
    function CountProps(const Name: string): Integer;
    // valeur d'une propri�t� par num�ro
    function ValNumProp(const Name: string; N: Integer; out Prop: string)
      : Boolean; overload;
    function ValNumProp(const Name: string; N: Integer): string;
    // liste des propri�t�s d'une liste
    function ListOfProps(const Name: string): string;
    // nom d'une propri�t� par num�ro
    function NameOfProp(const Name: string; N: Integer): string; overload;
    // changement dans la liste de propri�t�s
    property OnChange: TNotifyEvent read fOnchange write fOnchange;
    // liste de propri�t�s par num�ro
    property ListPByNum[N: Integer]: string read GetLPByNum; default;
    // liste de propri�t�s par nom
    property LPByName[const Name: string]: string read GetLPByName
       write SetLPByName;
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
  end;

implementation

uses
  StrUtils,
  lazutf8; // traitement des cha�nes UTF8

{ TGVPropListEnumerator }

function TGVPropListEnumerator.GetCurrent: string;
// *** retourne l'�l�ment courant ***
begin
  Result := fLst.Names[fIndex];
end;

constructor TGVPropListEnumerator.Create(const Value: TStrings);
// *** cr�ation de l'�num�rateur ***
begin
  fIndex := -1; // on pointe avant le premier �l�ment
  fLst := TStringList.Create; // liste interne
  fLst.NameValueSeparator := CSep; // d�finit le caract�re s�parateur
  fLst.AddStrings(Value); // on int�gre les valeurs � �num�rer
end;

destructor TGVPropListEnumerator.Destroy;
// *** destruction de l'�num�rateur ***
begin
  fLst.Free; // liste interne lib�r�e
  inherited Destroy; // on h�rite
end;

function TGVPropListEnumerator.MoveNext: Boolean;
// *** passe � l'�l�ment suivant ***
begin
  Result := fIndex < (fLst.Count - 1); // on v�rifie la borne sup�rieure
  if Result then // si OK
    Inc(fIndex); // on passe � l'�l�ment suivant
end;

{ ========================================================== }

{ TGVPropList }

function TGVPropList.GetLPByNum(N: Integer): string;
// *** renvoie la liste compl�te ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  if (N > 0) and (N <= fNames.Count) then // intervalle possible ?
    Result := fNames[N - 1]
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownNumListP, IntToStr(N));
end;

procedure TGVPropList.Change;
// *** changement dans la liste ***
begin
  if Assigned(fOnchange) then // gestionnaire actif ?
    fOnchange(Self); // on l'ex�cute
end;

procedure TGVPropList.Clear;
// *** nettoie les listes de propri�t�s ***
begin
  fNames.Clear; // pas de noms
  Error.Clear; // pas d'erreur
  Change; // changement notifi�
end;

function TGVPropList.CountListP: Integer;
// *** nombre de listes de propri�t�s ***
begin
  Result := fNames.Count; // = nombre de noms enregistr�s
end;

function TGVPropList.CountProps(const Name: string): Integer;
// *** nombre de propri�t�s attach�es � une liste ***
var
  LL: TGVList;
begin
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
    LL := TGVList.Create; // cr�ation de la liste provisoire
    try
      LL.Text := fNames.Values[Name]; // analyse de la liste
      Result := LL.Count div 2; // compte les �l�ments (par paires)
    finally
      LL.Free; // lib�ration de la liste de travail
    end;
  end
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

constructor TGVPropList.Create;
// *** constructeur de la classe ***
begin
  inherited Create; // on h�rite
  fNames := TStringList.Create; // on cr�e la liste interne
  fNames.NameValueSeparator := CSep; // on d�finit le caract�re s�parateur
  fOnchange := nil; // on met � nil le gestionnaire de changement
  fError := TGVErrors.Create; // cr�ation du gestionnaire d'erreurs
end;

destructor TGVPropList.Destroy;
// *** destructeur de la classe ***
begin
  fOnchange := nil; // gestionnaire d'�v�nements annul�
  fNames.Free; // on lib�re les noms
  fError.Free; // on lib�re le gestionnaire d'erreurs
  inherited Destroy; // on h�rite
end;

function TGVPropList.GetEnumerator: TGVPropListEnumerator;
// *** mise en place de l'�num�ration ***
begin
  Result := TGVPropListEnumerator.Create(fNames);
end;

function TGVPropList.GetLPByName(const Name: string): string;
// *** liste de propri�t�s par nom ***
var
  Li: Integer;
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  Li := fNames.IndexOfName(Name); // recherche de l'existence
  if Li <> -1 then // si trouv�e
    Result := fNames[Li] // on renvoie sa valeur
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

procedure TGVPropList.SetLPByName(const Name, Value: string);
// *** d�finit une liste de propri�t�s directement ***
begin
  fNames.Values[Name] := Value;
end;

function TGVPropList.IsListPByNum(N: Integer): Boolean;
// *** la propri�t� N existe-t-elle ? ***
begin
  Result := (N > 0) and (N <= fNames.Count); // dans les bornes ?
end;

function TGVPropList.IsListP(const Name: string): Boolean;
// *** la liste existe-t-elle ? ***
begin
  Result := (fNames.IndexOfName(Name) <> -1);
end;

function TGVPropList.IsProp(const Name, Prop: string): Boolean;
// *** la propri�t� existe-t-elle ? ***
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
        // cherche les �l�ments et teste la propri�t�
        Result := Odd(Li) and (UTF8CompareText(LL[Li - 1], Prop) = 0);
        if Result then
          Break; // on a trouv�, donc on sort
      end;
    finally
      LL.Free; // lib�ration de la liste de travail
    end;
  end
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.ListOfProps(const Name: string): string;
// *** liste des propri�t�s d'une liste ***
var
  Li: Integer;
  LL: TGVList;
begin
  Result := CBeginList; // d�but de la liste
  try
    if IsListP(Name) then
    begin
      LL := TGVList.Create; // cr�ation de la liste de travail
      try
        LL.Text := ValListP(Name); // r�cup�re la liste
        for Li := 1 to LL.Count do // on balaie les propri�t�s
          if Odd(Li) then // propri�t�s par paires
            Result := Result + LL[Li - 1] + CBlank; // on ajoute le nom
      finally
        LL.Free; // lib�ration de la liste de travail
      end;
    end
    else
      // [### Erreur: liste de propri�t�s inexistante ###]
      Error.SetError(CE_UnknownListP, Name);
  finally
    Result := TrimRight(Result) + CEndList; // fin de la liste
  end;
end;

function TGVPropList.ListP: string;
// *** renvoie la liste des listes de propri�t�s ***
var
  LSt: string;
begin
  Result := CBeginList;
  try
    for LSt in fNames do // on balaie la liste
      if (Result <> CBeginList) then
        Result := Result + CBlank + LSt // pour construire la cha�ne
      else
        Result := Result + LSt;
    // on remplace le caract�re s�parateur
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
  LLst.NameValueSeparator := CSep; // s�parateur pour les propri�t�s
  try
    // change l'extension si n�cessaire
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
    LLst.Delete(0); // on �limine l'ent�te
    LL := TGVList.Create; // liste interm�diaire cr��e
    try
      for Li := 0 to LLst.Count - 1 do // on la balaie
      begin
        if Error.OK then // on met � jour si pas d'erreur
        begin
          GVName := LLst.Names[Li]; // on a le nom
          LL.Text := LLst.Values[GVName]; // et les propri�t�s
          for Lj := 1 to LL.Count do
            if Odd(Lj) then
              UpDateListP(GVName, LL[Lj - 1], LL[Lj]); // on met � jour
        end;
      end;
    finally
      LL.Free; // on lib�re la liste interm�diaire
    end;
  finally
    LLst.Free; // on lib�re la liste de travail
  end;
end;

function TGVPropList.NameOfProp(const Name: string; N: Integer): string;
// *** nom d'une propri�t� par num�ro ***
var
  LL: TGVList;
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  if IsListP(Name) then // est-ce une liste de propri�t�s ?
  begin
    LL := TGVList.Create;
    try
      if (N > 0) and (N <= CountProps(Name)) then // dans les bornes ?
      begin
        LL.Text := ValListP(Name); // liste de travail
        Result := LL[(N - 1) * 2]; // propri�t�s par paires
      end
      else
       // [### Erreur: propri�t� inexistante ###]
       Error.SetError(CE_UnknownNumProp, IntToStr(N));
    finally
      LL.Free; // on lib�re la liste
    end;
  end
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.NumListP(const Name: string): Integer;
// *** recherche d'une liste de propri�t�s ***
begin
  Result := fNames.IndexOfName(Name) + 1;
  if Result = 0 then
    Result := -1; // non trouv�e
end;

function TGVPropList.NumProp(const Name, Prop: string): Integer;
// *** recherche du num�ro d'une propri�t� ***
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
        // cherche les �l�ments et teste la propri�t�
        if Odd(Li) and (UTF8CompareText(LL[Li - 1], Prop) = 0) then
        begin
          Result := ((Li - 1) div 2) + 1; // toujours par paires
          Break; // on a trouv�, donc on sort
        end;
    finally
      LL.Free; // lib�ration de la liste de travail
    end;
    if Result = -1 then // propri�t� non trouv�e
      // [### Erreur: propri�t� inexistante ###]
      Error.SetError(CE_UnknownProp, Prop);
  end
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.RemoveListP(const Name: string): Boolean;
// *** d�truit la liste de propri�t�s ***
var
  Li: Integer;
begin
  Result := False; // suppose une erreur
  Li := fNames.IndexOfName(Name); // on cherche la liste
  if (Li <> -1) then
  begin
    fNames.Delete(Li); // on la d�truit
    Result := True;
    Change; // on notifie les changements
  end
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.RemoveProp(const Name, Prop: string): Boolean;
// *** d�truit une propri�t� ***
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
        // on retire deux �l�ments si possible
        if IsProp(Name, Prop) then
        begin
          fNames.Values[Name] := LL.TwoDelete(NumProp(Name, Prop) * 2 - 1);
          if ValListP(Name) = LUtil.EmptyList then
            // on d�truit la liste si vide
            Result := RemoveListP(Name)
          else
            Result := True;
          Change; // on notifie les changements
        end
        else
          // [### Erreur: propri�t� inexistante ###]
          Error.SetError(CE_UnknownProp, Prop);
      finally
        LL.Free; // on lib�re la liste de travail
      end;
    finally
      LUtil.Free;
    end;
  end
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

procedure TGVPropList.SaveToFile(const FileName: string);
// *** sauvegarde des listes de propri�t�s ***
var
  LL: TStringList;
  LSt: string;
begin
  LL := TStringList.Create; // liste de travail
  try
    // change l'extension si n�cessaire
    if PosEx(CDot, FileName) = 0 then
      LSt := FileName + CExtPl
    else
      LSt := FileName;
    LL.Add(CHeader); // ajoute l'ent�te
    LL.AddStrings(fNames); // ajoute les listes
    try
      LL.SaveToFile(LSt); // sauve la liste
    except
      // [### Erreur: mauvaise sauvegarde ###]
      Error.SetError(CE_BadSave, LSt);
    end;
  finally
    LL.Free; // lib�re la liste de travail
  end;
end;

function TGVPropList.UpDateListP(const Name, Prop, Value: string): Boolean;
// *** cr�e ou met � jour la liste de propri�t�s ***
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
      if not Result then // nom de propri�t� incorrect ?
      begin
        // [### Erreur: mot incorrect ###]
        Error.SetError(CE_BadWord, Prop);
        Exit; // on sort
      end;
      Result := LUtil.IsValidValue(Value);
      if not Result then  // valeur de propri�t� incorrecte ?
      begin
        // [### Erreur: ni une liste ni un mot ###]
        Error.SetError(CE_UnknownListWord, Value);
        Exit; // on sort
      end
      else // si tout est Ok
      begin
        LS := CBeginList + Value + CEndList; // valeur en liste
        if IsListP(Name) then // la liste existe-t-elle d�j� ?
        begin // oui
          LL := TGVList.Create;
          try
            LL.Text := ValListP(Name); // cherche les propri�t�s
            if IsProp(Name, Prop) then
            begin
              Li := NumProp(Name, Prop); // num�ro de la propri�t�
              // on remplace
              fNames.Values[Name] := LL.ReplaceItem(2 * Li, LS);
            end
            else
              fNames.Values[Name] := LL.TwoAdd(Prop, LS); // on ajoute
            Change; // notifie le changement
          finally
            LL.Free; // on lib�re la liste de travail
          end;
        end
        else // cr�ation de la nouvelle liste
        begin
          fNames.Add(Name + fNames.NameValueSeparator + CBeginList + Prop +
            CBlank + LS + CEndList); // on cr�e le nom
          Change; // notifie le changement
        end;
      end;
    finally
      LWord.Free; // lib�ration du mot de travail
    end;
  finally
    LUtil.Free; // lib�ration de la cha�ne utilitaire
  end;
end;

function TGVPropList.ValListP(const Name: string): string;
// *** valeur d'une liste de propri�t�s ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  if IsListP(Name) then // la liste existe-t-elle ?
    Result := fNames.Values[Name] // retourne les propri�t�s
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.ValNumListP(N: Integer; out Name, Value: string): Boolean;
// *** valeur d'une liste de propri�t�s par num�ro ***
begin
  Result := False; // suppose une erreur
  if (N > 0) and (N <= fNames.Count) then // dans les bornes ?
  begin
    Name := fNames.Names[N-1]; // d'abord le nom
    Value := fNames.ValueFromIndex[N-1]; // puis les propri�t�s
    Result := True; // tout va bien
  end
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownNumListP, IntToStr(N));
end;

function TGVPropList.ValNumProp(const Name: string; N: Integer): string;
// *** valeur d'une propri�t� par num�ro *** ###
var
  LL: TGVList;
  LU: TGVListUtils;
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
    LL := TGVList.Create;
    try
      LU := TGVListUtils.Create; // utilitaire
      try
      if (N > 0) and (N <= CountProps(Name)) then // dans les bornes ?
      begin
        LL.Text := ValListP(Name); // liste de travail
        // propri�t�s par paires, valeur sans les crochets
        Result := LU.ListToStr(LL[(N - 1) * 2]);
      end
      else
        // [### Erreur: propri�t� inexistante ###]
        Error.SetError(CE_UnknownNumProp, IntToStr(N));
      finally
        LU.Free; // utilitaire lib�r�
      end;
    finally
      LL.Free; // on lib�re la liste
    end;
  end
  else
    // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.ValNumProp(const Name: string; N: Integer;
  out Prop: string): Boolean;
// *** valeur d'une propri�t� par num�ro ***
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
          Prop := LU.ListToStr(LL[(N - 1) * 2]); // propri�t�s par paires
          Result := True; // tout est Ok
        end
        else
         // [### Erreur: propri�t� inexistante ###]
         Error.SetError(CE_UnknownNumProp, IntToStr(N));
      finally
        LU.Free; // utilitaire lib�r�
      end;
    finally
      LL.Free; // on lib�re la liste
    end;
  end
  else
   // [### Erreur: liste de propri�t�s inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

function TGVPropList.ValProp(const Name, Prop: string): string;
// *** valeur d'une propri�t� ***
var
  LL: TGVList;
  Li: Integer;
  LU: TGVListUtils;
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  if IsListP(Name) then // la liste existe-t-elle ?
  begin
  LL := TGVList.Create;
  try
    if IsProp(Name, Prop) then
    begin
      LL.Text := ValListP(Name); // liste de travail
      LU := TGVListUtils.Create; // cr�ation de l'utilitaire
      try
      for Li := 1 to LL.Count do // on examine la liste
        // si c'est une propri�t� et qu'elle correspond � celle cherch�e
        if Odd(Li) and (UTF8CompareText(LL[Li - 1], Prop) = 0) then
        begin
          Result := Lu.ListToStr(LL[Li]); // �l�ment trouv�
          Break; // on sort de la boucle
        end;
      finally
        LU.Free; // lib�ration de l'utilitaire
      end;
    end
    else
      // [### Erreur: propri�t� inexistante ###]
      Error.SetError(CE_UnknownProp, Prop);
  finally
    LL.Free; // on lib�re la liste
  end;
  end
  else
    // [### Erreur: propri�t� inexistante ###]
    Error.SetError(CE_UnknownListP, Name);
end;

end.
