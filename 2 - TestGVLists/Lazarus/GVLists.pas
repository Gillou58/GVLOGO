{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Listes                                  |
  |                  Unit� : GVLists.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR                              |
  |                  Date:    30-06-2014 16:30:23                          |
  |                  Version : 2.0.0                                       |
  |                                                                        |
  |========================================================================| }

 {.$DEFINE Delphi}
unit GVLists;

// Unit� pour le traitement des listes
//
// ##############################################################
//
// LISTES SIMPLES
//
// 1. Un mot est un ensemble de caract�res autres que le caract�re blanc.
// 2. Une liste est un ensemble de mots plac�s entre crochets : [ et ].
// 3. Une liste peut comprendre d'autres listes.
// 4. Un �l�ment d'une liste est soit un mot soit une liste.
// 5. Il existe une liste qui ne comprend aucun �l�ment et qu'on appelle
// la liste vide.
//
// Exemples :
//
// [coucou] est une liste valide.
// [coucou [encore coucou]] est une liste valide.
// [coucou coucou2] est une liste valide.
// [[coucou]] est une liste valide.
// [[coucou] n'est pas une liste valide (il manque un crochet fermant).
//
// Les listes sont � la base du langage GVLogo.
// Elles permettent un traitement des cha�nes de caract�res � partir de
// primitives simples.
// Les lignes d'un programme en GVLogo sont consid�r�es comme des listes.
//
// On notera que les �l�ments sont num�rot�s de 0 � Count-1.
// Cette fa�on de compter est conforme aux listes de Pascal.
//

interface

uses
  Classes, SysUtils,
  GVConsts, // constantes
  GVWords; // mots

type
   TGVListUtils = class; // pour d�finition ult�rieure

  { classe des listes }

  { TGVList }

  EGVListException = class(Exception);

  TGVList = class(TStringList)
  strict private
    fError: TGVError; // erreur
    fErrorPos: Integer; // position d'une erreur
    fLoading: Boolean; // chargement en cours ?
    fNumLastItem: Integer; // dernier �l�ment trouv�
    fWord: TGVWord; // mot de travail
    fUtil: TGVListUtils; // utilitaire pour liste
    function GetLastErrorPos: Integer;  // position de la derni�re erreur
    function GetLastError: TGVError; // derni�re erreur
  protected
    procedure Put(Index: Integer; const S: string); override; // assignation
  public
    constructor Create; overload; // cr�ation
    destructor Destroy; override; // destruction
    function Add(const St: string): Integer; override; // ajout
    procedure LoadFromStream(Stream: TStream); overload; override; // chargement
    procedure Assign(Source: TPersistent); override; // assignation
    procedure Insert(Index: Integer; const S: string); override; // insertion
    // nouvelles m�thodes (*** ne modifient pas la liste interne ***)
    // renvoie la liste sous forme de cha�ne
    function ToStr: string;
    // renvoie la liste sous forme de cha�ne sans crochets
    function ToWBStr: string;
    // la liste est-elle vide ?
    function IsEmpty: Boolean;
    // la liste est-elle la liste vide ?
    function IsEmptyList: Boolean;
    // renvoie le premier �l�ment de la liste
    function First: string;
    // renvoie le dernier �l�ment de la liste
    function Last: string;
    // sauf le premier de la liste
    function ButFirst: string;
    // sauf le dernier de la liste
    function ButLast: string;
    // supprime l'�l�ment N
    function DeleteItem(N: Integer): string;
    // insertion d'un �l�ment en position N
    function InsertAItem(N: Integer; const St: string): string;
    // remplacement de l'�l�ment N
    function ReplaceItem(N: Integer; const St: string): string;
    // met en premier
    function PutFirst(const St: string): string;
    // met en dernier
    function PutLast(const St: string): string;
    // phrase � droite
    function SentenceRight(const St: string): string;
    // phrase � gauche
    function SentenceLeft(const St: string): string;
    // tri des �l�ments
    function SortItems: string;
    // inversion des �l�ments
    function ReverseItems: string;
    // m�lange des �l�ments
    function ShuffleItems: string;
    // membre pr�sent ?
    function IsItem(const St: string): Boolean;
    // ajout d'une paire
    function TwoAdd(const St1, St2: string): string;
    // suppression d'une paire
    function TwoDelete(N: Integer): string;
    // liste en majuscules
    function UpperCase: string;
    // liste en minuscules
    function LowerCase: string;
    // rotation de la liste
    function Rotate: string;
    // nouvelles propri�t�s
    // derni�re erreur
    property LastError: TGVError read GetLastError default C_None;
    // position de la derni�re erreur
    property LastErrorPos: Integer read GetLastErrorPos default -1;
    // dernier �l�ment trait�
    property LastItem: Integer read fNumLastItem default -1;
  end;

  { classe des utilitaires pour les listes}

  { TGVListUtils }

  EGVListUtilsException = class(Exception);

  TGVListUtils = class
  strict private
    fWord: TGVWord; // mot de travail
    fSt: TGVString; // cha�ne de travail
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    // conversion d'une liste en mot
    function ListToWord(const St: string): string;
    // conversion d'un mot en liste
    function WordToList(const St: string): string;
    // conversion d'une liste en cha�ne
    function ListToStr(const St: string): string;
    // conversion d'une cha�ne en liste
    function StrToList(const St: string): string;
    // retourne la liste vide
    function EmptyList: string;
    // v�rifie la validit� d'une liste
    function IsValid(const St: string): Boolean;
    // teste la validit� d'une valeur (mot ou liste)
    procedure TestValue(const St: string);
    // teste la validit� d'une valeur (mot ou liste) - sans exception
    function IsValidValue(const St: string): Boolean;
    // liste simple ?
    function IsSimpleList(const St: string): Boolean;
  end;

implementation

{$IFNDEF Delphi}
  uses
    lazutf8; // traitement des cha�nes UTF8
{$ENDIF}


{ TGVList }

function TGVList.Add(const St: string): Integer;
// *** ajout d'une cha�ne ***
var
  S: string;
  fStrList: TStringList; // liste de travail

  {$IFDEF Delphi}
  procedure Parse(const St: string);
  // *** d�coupe la liste de travail ***
  var
    LW: Integer; // place du caract�re examin�
    Lev: Integer; // niveau de travail
    LItem: string; // �l�ment en cours
  begin
    fErrorPos := 0; // pas d'erreur
    fStrList.Clear; // on nettoie la liste de travail
    if St = EmptyStr then // si cha�ne vide, on l'enregistre
    begin
      fStrList.Add(EmptyStr);
      exit; // ... et on sort
    end;
    LItem := EmptyStr; // �l�ment en cours vide
    LW := 1; // pointe sur le premier caract�re
    while (fErrorPos = 0) and (LW <= Length(St)) do // on boucle si possible
    begin
      case St[LW] of // examen du caract�re en cours
        CBlank: // *** blanc ? ***
          begin
            Inc(LW); // pointe sur le caract�re suivant
            while (St[LW] = CBlank) and (LW <= Length(St)) do
               Inc(LW); // saute les blancs
            if LItem <> EmptyStr then
            begin
              fStrList.Add(LItem); // ajoute l'�l�ment si non vide
              LItem := EmptyStr; // �l�ment � z�ro
            end;
          end;
        CBeginList: // *** d�but d'une sous-liste ? ***
          begin
            if LItem <> EmptyStr then // ajoute l'�l�ment si non vide
              fStrList.Add(LItem);
            LItem := CBeginList; // �lement = [
            Inc(LW); // pointe sur le caract�re suivant
            Lev := 1; // niveau 1
            while (Lev <> 0) and (LW <= Length(St)) do // autres niveaux ?
            begin
              case St[LW] of
                CLink: // un lien ?
                  begin
                    LItem := LItem + CLink; // dans l'�l�ment
                    Inc(LW); // pointe sur le caract�re suivant
                  end;
                CBeginList:
                  Inc(Lev); // niveau suivant
                CEndList:
                  Dec(Lev); // niveau pr�c�dent
              end;
              if LW <= Length(St) then // si possible
                LItem := LItem + St[LW]; // sauve l'�l�ment suivant
              Inc(LW); // pointe sur le caract�re suivant
            end; // fin des niveaux
            if Lev = 0 then // normalement niveau = 0 � la sortie
            begin // c'est-�-dire autant de [ que de ]
              fStrList.Add(LItem); // sauve le dernier �l�ment
              LItem := EmptyStr; // et le remet � z�ro
            end
            else // si niveaux incorrects
              fErrorPos := LW; // note la position de l'erreur
          end;
        CBeginPar: // *** d�but de parenth�se ?
          begin
            if LItem <> EmptyStr then // sauve l'�l�ment si non vide
              fStrList.Add(LItem);
            LItem := CBeginPar; // �lement = (
            Inc(LW); // pointe sur le caract�re suivant
            Lev := 1; // premier niveau
            while (Lev <> 0) and (LW <= Length(St)) do // autres niveaux ?
            begin
              case St[LW] of
                CLink: // un lien ?
                  begin
                    LItem := LItem + CLink; // dans l'�l�ment
                    Inc(LW); // pointe sur le caract�re suivant
                  end;
                CBeginList, CEndList:
                  fErrorPos := LW; // pas de liste dans une expression
                CBeginPar:
                  Inc(Lev); // niveau suivant
                CEndPar:
                  Dec(Lev); // niveau pr�c�dent
              end;
              if LW <= Length(St) then // si possible
                LItem := LItem + St[LW]; // dans l'�l�ment
              Inc(LW); // pointe sur le caract�re suivant
            end; // fin des autres niveaux
            if (Lev = 0) and (fErrorPos = 0) then // nombre correct de niveaux ?
            begin
              fStrList.Add(LItem); // sauve le dernier �l�ment
              LItem := EmptyStr; // remis � z�ro
            end
            else // si erreur
              if fErrorPos = 0 then
                fErrorPos := LW;
            // note la position de l'erreur si non connue
          end;
        CLink: // *** un lien ? ***
          begin
            LItem := LItem + CLink; // dans l'�l�ment en cours
            Inc(LW); // pointe sur le caract�re suivant
            if LW <= Length(St) then // si possible
            begin
              LItem := LItem + St[LW]; // dans l'�l�ment en cours
              Inc(LW); // pointe sur le caract�re suivant
            end;
          end;
        CEndList, CEndPar: // *** fin de liste ou parenth�se ? => erreur ***
          fErrorPos := LW; // position de l'erreur
      else // autres caract�res � ajouter simplement
        begin
          LItem := LItem + St[LW]; // dans l'�l�ment en cours
          Inc(LW); // pointe sur le caract�re suivant
        end;
      end;
    end; // fin de la boucle
    if (fErrorPos = 0) and (LItem <> EmptyStr) then // pas d'erreur ?
      fStrList.Add(LItem); // on sauve le dernier �l�ment �ventuel
  end;
  {$ELSE}
  procedure Parse(const St: string);
  // *** d�coupe la liste de travail ***
  var
    LW: Integer; // place du caract�re examin�
    Lev: Integer; // niveau de travail
    LItem: string; // �l�ment en cours
    S: string; // caract�re en cours (1 ou 2)
  begin
    fErrorPos := 0; // pas d'erreur
    fStrList.Clear; // on nettoie la liste de travail
    if St = EmptyStr then // si cha�ne vide, on l'enregistre
    begin
      fStrList.Add(EmptyStr);
      exit; // ... et on sort
    end;
    LItem := EmptyStr; // �l�ment en cours vide
    LW := 1; // pointe sur le premier caract�re
    S := UTF8Copy(St, LW, 1);
    while (fErrorPos = 0) and (LW <= UTF8Length(St)) do // on boucle si possible
    begin
      if S = CBlank then // *** blanc ?
      begin
        Inc(LW); // pointe sur le caract�re suivant
        S := UTF8Copy(St, LW, 1);
        while (S = CBlank) and (LW <= UTF8Length(St)) do
        begin
          Inc(LW); // saute les blancs
          S := UTF8Copy(St, LW, 1);
        end;
        if LItem <> EmptyStr then
        begin
          fStrList.Add(LItem); // ajoute l'�l�ment si non vide
          LItem := EmptyStr; // �l�ment � z�ro
        end;
      end
      else
      if S = CBeginList then // *** d�but d'une sous-liste ? ***
      begin
        if LItem <> EmptyStr then // ajoute l'�l�ment si non vide
          fStrList.Add(LItem);
        LItem := CBeginList; // �lement = [
        Inc(LW); // pointe sur le caract�re suivant
        S := UTF8Copy(St, LW, 1);
        Lev := 1; // niveau 1
        while (Lev <> 0) and (LW <= UTF8Length(St)) do // autres niveaux ?
        begin
          if S = CLink then // un lien ?
          begin
            LItem := LItem + CLink; // dans l'�l�ment
            Inc(LW); // pointe sur le caract�re suivant
            S := UTF8Copy(St, LW, 1);
          end
          else
          if S = CBeginList then
            Inc(Lev) // niveau suivant
          else
          if S = CEndList then
            Dec(Lev); // niveau pr�c�dent
          if LW <= UTF8Length(St) then // si possible
                LItem := LItem + S; // sauve l'�l�ment suivant
          Inc(LW); // pointe sur le caract�re suivant
          S := UTF8Copy(St, LW, 1);
       end; // fin des niveaux
       if Lev = 0 then // normalement niveau = 0 � la sortie
       begin // c'est-�-dire autant de [ que de ]
         fStrList.Add(LItem); // sauve le dernier �l�ment
         LItem := EmptyStr; // et le remet � z�ro
       end
       else // si niveaux incorrects
         fErrorPos := LW; // note la position de l'erreur
      end
      else
      if S = CBeginPar then // *** d�but de parenth�se ?
      begin
        if LItem <> EmptyStr then // sauve l'�l�ment si non vide
          fStrList.Add(LItem);
        LItem := CBeginPar; // �lement = (
        Inc(LW); // pointe sur le caract�re suivant
        S := UTF8Copy(St, LW, 1);
        Lev := 1; // premier niveau
        while (Lev <> 0) and (LW <= UTF8Length(St)) do // autres niveaux ?
        begin
          if S = CLink then // un lien ?
          begin
            LItem := LItem + CLink; // dans l'�l�ment
            Inc(LW); // pointe sur le caract�re suivant
            S := UTF8Copy(St, LW, 1);
          end
          else
          if (S = CBeginList) or (S = CEndList) then
            fErrorPos := LW // pas de liste dans une expression
          else
          if S = CBeginPar then
            Inc(Lev) // niveau suivant
          else
          if S = CEndPar then
            Dec(Lev); // niveau pr�c�dent
          if LW <= UTF8Length(St) then // si possible
            LItem := LItem + S; // dans l'�l�ment
          Inc(LW); // pointe sur le caract�re suivant
          S := UTF8Copy(St, LW, 1);
        end; // fin des autres niveaux
        if (Lev = 0) and (fErrorPos = 0) then // nombre correct de niveaux ?
        begin
          fStrList.Add(LItem); // sauve le dernier �l�ment
          LItem := EmptyStr; // remis � z�ro
        end
        else // si erreur
          if fErrorPos = 0 then
            fErrorPos := LW; // note la position de l'erreur si non connue
      end
      else
      if S = CLink then // *** un lien ? ***
      begin
        LItem := LItem + CLink; // dans l'�l�ment en cours
        Inc(LW); // pointe sur le caract�re suivant
        S := UTF8Copy(St, LW, 1);
        if LW <= UTF8Length(St) then // si possible
        begin
          LItem := LItem + S; // dans l'�l�ment en cours
          Inc(LW); // pointe sur le caract�re suivant
          S := UTF8Copy(St, LW, 1);
        end;
      end
      else
      if (S = CEndList) or (S = CEndPar) then // *** fin de liste ou parenth�se ? => erreur ***
        fErrorPos := LW // position de l'erreur
      else // *** autres caract�res � ajouter simplement ***
      begin
        LItem := LItem + S; // dans l'�l�ment en cours
        Inc(LW); // pointe sur le caract�re suivant
        S := UTF8Copy(St, LW, 1);
      end;
    end; // fin de la boucle
    if (fErrorPos = 0) and (LItem <> EmptyStr) then // pas d'erreur ?
      fStrList.Add(LItem); // on sauve le dernier �l�ment �ventuel
  end;
  {$ENDIF}

begin
  BeginUpdate; // on marque le changement en cours
  fError := C_BadList; // on suppose une erreur
  Result := -1;
  fStrList := TStringList.Create; // on cr�e la liste de travail
  try
    // est-ce un mot ?
    if fWord.IsValid(St) then
    begin
      Result := inherited Add(St); // on l'ins�re
      fError := C_None;
    end
    {$IFDEF Delphi}
    else if (Length(St) > 1) and (St[1] = CBeginList)
      and (St[Length(St)] = CEndList) then // on veut les crochets
        // recherche des �l�ments d'une liste
    begin
      Parse(Copy(St, 2, Length(St) - 2));
    {$ELSE}
    else if (UTF8Length(St) > 1) and (St[1] = CBeginList)
      and (St[Length(St)] = CEndList) then
    begin
      Parse(UTF8Copy(St, 2, UTF8Length(St) - 2));
    {$ENDIF}
      if fErrorPos = 0 then // pas d'erreur pour le parsing
      begin
        for S in fStrList do // on ajoute tous les �l�ments de la liste
          Result := inherited Add(S);
        fError := C_None;
      end
    end
    else
      fErrorPos := 1; // erreur d�s le d�part
  finally
    fStrList.Free;
    EndUpdate; // fin de tout changement
  end;
end;

function TGVList.ButFirst: string;
// *** renvoie tout sauf le premier �l�ment de la liste ***
// BF [mot1 mot2] => [mot2]
// BF [[sous-liste1] mot2 mot3 [sous-liste2]] => [mot2 mot3 [sous-liste2]]
begin
  Result := DeleteItem(1); // on enl�ve le dernier �l�ment
end;

function TGVList.ButLast: string;
// *** renvoie tout sauf le dernier �l�ment de la liste ***
// BL [mot1 mot2] => [mot1]
// BL [[sous-liste1] mot2 mot3 [sous-liste2]] => [[sous-liste1] mot2 mot3]
begin
  Result := DeleteItem(Count); // on d�truit le dernier �l�ment
end;

constructor TGVList.Create;
// *** cr�ation de la liste ***
begin
  inherited Create; // on h�rite
  fNumLastItem := -1; // dernier �l�ment recherch�
  fError := C_None; // derni�re erreur
  fErrorPos := -1; // derni�re position d'erreur dans liste
  fWord := TGVWord.Create; // cr�ation du mot de travail
  fUtil := TGVListUtils.Create; // utilitaires de travail
end;

destructor TGVList.Destroy;
// *** destruction de la liste ***
begin
  fWord.Free; // lib�ration du mot de travail
  fUtil.Free; // idem pour les utilitaires
  inherited Destroy; // on h�rite
end;

function TGVList.DeleteItem(N: Integer): string;
// *** d�truit l'�l�ment N de la liste ***
// DL 1 [mot1 mot2] => [mot2]
// DL 3 [[sous-liste1] mot2 mot3 [sous-liste2]] => [[sous-liste1] mot2 [sous-liste2]]
var
  I: Integer;
begin
  Result := CBeginList; // crochet ouvrant
  try
    if (N > Count) or (N < 1) then // les �l�ments existent-ils ?
      raise EGVListException.CreateFmt(ME_DelItem, [N]); // erreur
    for I := 1 to Count do // on reconstruit la liste
      if (I <> N) then // sans l'�l�ment N
        Result := Result + Get(I-1) + CBlank;
  finally
    Result := TrimRight(Result) + CEndList; // et on termine par le crochet fermant
  end;
end;

function TGVList.First: string;
// *** premier �l�ment de la liste ***
// FL [mot1 mot2] => mot1
// CL [[sous-liste1] mot2 mot3 [sous-liste2]] => [sous-liste1]
begin
  Result := Get(0); // on renvoie le premier �l�ment
end;

function TGVList.GetLastError: TGVError;
// *** renvoie la derni�re erreur ***
begin
  Result := fError; // renvoi de l'erreur
  fError := C_None; // remise � z�ro de l'erreur
end;

function TGVList.InsertAItem(N: Integer; const St: string): string;
// *** ins�re un �l�ment � la position N ***
// II 2 mot0 [mot1 mot2] => [mot1 mot0 mot2]
// II 3 mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [[sous-liste1] mot2 mot0 mot3 [sous-liste2]]
var
  I: Integer;
  S1, S2: string;
begin
  Result := CBeginList; // crochet ouvrant
  try
    fUtil.TestValue(St); // liste ou mot valides ?
    if (N > (Count + 1 )) or (N < 1) then // les �l�ments existent-ils ?
      raise EGVListException.CreateFmt(ME_InsItem, [N]); // erreur
    for I := 1 to Count + 1 do // on reconstruit la liste
    begin
      if (I <= Count) then
        S1 := Get(I-1)
      else
        S1 := EmptyStr;
      if I = N then // c'est le point d'insertion ?
      begin
        S2 := St;
        if ((Result <> CBeginList) or (S2 <> EmptyStr)) and (S1 <> EmptyStr)
        then
          S1 := CBlank + S1;
        if (Result <> CBeginList) and (S2 <> EmptyStr) then
          S2 := CBlank + S2;
        Result := Result + S2 + S1; // on ins�re...
      end
      else
      begin
        if (Result <> CBeginList) and (S1 <> EmptyStr) then
          S1 := CBlank + S1;
        Result := Result + S1; // on ajoute l'�l�ment suivant
      end;
    end;
  finally
    Result := Result + CEndList; // et on termine par le crochet fermant
  end;
end;

procedure TGVList.Insert(Index: Integer; const S: string);
// *** insertion d'un objet ***
begin
  BeginUpdate; // d�but de changement
  try
    fUtil.TestValue(S); // mot ou liste ?
    inherited Insert(Index, S); // on ins�re la valeur
  finally
    EndUpdate; // fin de changement
  end;
end;

function TGVList.IsEmpty: Boolean;
// *** la liste est-elle vide ? ***
begin
  Result := (Count = 0);
end;

function TGVList.IsEmptyList: Boolean;
// *** la liste est-elle la liste vide ? ***
begin
  Result := (Count = 1) and (Get(0) = EmptyStr);
end;

function TGVList.IsItem(const St: string): Boolean;
// *** l'�l�ment donn� est-il dans la liste ? ***
// II? mot0 [mot1 mot2] => faux
// II? mot2 [[sous-liste1] mot2 mot3 [sous-liste2]] => vrai
begin
  fNumLastItem := IndexOf(St);
  Result := (fNumLastItem <> -1);
end;

function TGVList.Last: string;
// *** renvoie le dernier �l�ment de la liste ***
// LL [mot1 mot2] => mot2
// LL [[sous-liste1] mot2 mot3 [sous-liste2]] => [sous-liste2]
begin
  Result := Get(Count - 1);
end;

procedure TGVList.LoadFromStream(Stream: TStream);
// *** chargement de la liste ***
begin
  fLoading := True; // on indique un chargement
  try
    inherited LoadFromStream(Stream); // on charge
  finally
    fLoading := False; // fin du chargement
  end;
end;

procedure TGVList.Assign(Source: TPersistent);
// *** assign surcharg�e ***
begin
  if Source is TGVList then // est-ce une liste ?
  begin
    BeginUpdate;
    try
      Text := CBeginList + ToWBStr + CBlank + TGVList(Source).ToWBStr +
        CEndList; // on ajoute les nouvelles donn�es
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TGVList.LowerCase: string;
// *** liste en minuscules ***
begin
{$IFDEF Delphi}
  Result := SysUtils.AnsiLowerCase(ToStr);
{$ELSE}
  Result := UTF8LowerCase(ToStr);
{$ENDIF}
end;

function TGVList.Rotate: string;
// *** rotation de la liste ***
var
  I: Integer;
begin
  Result := CBeginList; // crochet ouvrant
  try
    for I := 1 to Count - 1 do // on reconstruit la liste
       Result := Result + Get(I) + CBlank; // on ajoute l'�l�ment suivant
    Result := Result + Get(0); // premier �l�ment � la fin
  finally
    Result := Result + CEndList; // et on termine par le crochet fermant
  end;
end;

function TGVList.GetLastErrorPos: Integer;
// *** position de la derni�re erreur ***
begin
  Result := fErrorPos;
  fErrorPos := 0;
end;

procedure TGVList.Put(Index: Integer; const S: string);
// *** changement direct d'un �l�ment ***
begin
  BeginUpdate;
  try
    fUtil.TestValue(S); // mot ou liste ?
    inherited Put(Index, S); // on change la valeur
  finally
    EndUpdate;
  end;
end;

function TGVList.PutFirst(const St: string): string;
// *** St comme premier �l�ment de la liste ***
// PF [mot0] [mot1 mot2] => [[mot0] mot1 mot2]
// PF mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [mot0 [sous-liste1] mot2 mot3 [sous-liste2]]
var
  S: string;
begin
  Result := CBeginList;
  try
    fUtil.TestValue(St); // liste ou mot valides
    S := ToWBStr; // liste en cha�ne
    if (S <> EmptyStr) and (St <> EmptyStr) then
      S := CBlank + S;
    // on construit la liste
    Result := Result + St + S;
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.PutLast(const St: string): string;
// *** St comme dernier �l�ment de la liste ***
// PL [mot0] [mot1 mot2] => [mot1 mot2 [mot0]]
// PL mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [[sous-liste1] mot2 mot3 [sous-liste2] mot0]
var
  S: string;
begin
  Result := CBeginList;
  try
    fUtil.TestValue(St); // liste ou mot valides
    S := ToWBStr; // liste en cha�ne
    if (S <> EmptyStr) and (St <> EmptyStr) then
      S := S + CBlank;
    // on construit la liste
    Result := Result + S + St;
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.ReplaceItem(N: Integer; const St: string): string;
// *** remplace l'�l�ment N dans la liste par St ***
// RI 1 mot0 [mot1 mot2] => [mot1 mot0]
// RI 2 mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [[sous-liste1] mot2 mot0 [sous-liste2]]
var
  I: Integer;
  S1, S2: string;
begin
  Result := CBeginList; // crochet ouvrant
  try
    fUtil.TestValue(St); // liste ou mot valides ?
    if (N > Count) or (N < 1) then // les �l�ments existent-ils ?
      raise EGVListException.CreateFmt(ME_ReplaceItem, [N]); // erreur
    for I := 1 to Count do // on reconstruit la liste
    begin
      if I = N then // c'est le point de remplacement ?
      begin
        S1 := St;
        if (Result <> CBeginList) and (S1 <> EmptyStr) then
          S1 := CBlank + S1;
        Result := Result + S1; // on remplace
      end
      else
      begin
        S2 := Get(I-1);
        if (Result <> CBeginList) and (S2 <> EmptyStr) then
          S2 := CBlank + S2;
        Result := Result + S2; // on ajoute l'�l�ment suivant
      end;
    end;
  finally
    Result := Result + CEndList; // et on termine par le crochet fermant
  end;
end;

function TGVList.SentenceLeft(const St: string): string;
// *** phrase avec valeur � gauche ***
// SL [mot0] [mot1 mot2] => [mot0 mot1 mot2]
// SL mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [mot0 [sous-liste1] mot2 mot3 [sous-liste2]]
var
  S: string;
begin
  Result := CBeginList;
  try
    S := ToWBStr;
    if (S <> EmptyStr) and (St <> EmptyStr) and (St <> CEmptyList) then
      S := CBlank + S;
    if fWord.IsValid(St) then // mot valide ?
      Result := Result + St + S
    else if fUtil.IsValid(St) then // ou liste valide ?
      Result := Result + Copy(St, 2, Length(St) - 2) + S
    else
      raise EGVListException.CreateFmt(ME_NoListWord, [St]); // erreur
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.SentenceRight(const St: string): string;
// *** phrase avec valeur � droite ***
// SR [mot0] [mot1 mot2] => [mot1 mot2 mot0]
// SR mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [[sous-liste1] mot2 mot3 [sous-liste2] mot0]
var
  S: string;
begin
  Result := CBeginList;
  try
    S := ToWBStr;
    if (S <> EmptyStr) and (St <> EmptyStr) and (St <> CEmptyList) then
      S := S + CBlank;
    if fWord.IsValid(St) then // mot valide ?
      Result := Result + S + St
    else if fUtil.IsValid(St) then // ou liste valide ?
      Result := Result + S + Copy(St, 2, Length(St) - 2)
    else
      raise EGVListException.CreateFmt(ME_NoListWord, [St]); // erreur
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.ShuffleItems: string;
// *** m�lange des �l�ments ***
var
  I: Integer;
  L: TGVList;
begin
  L := TGVList.Create; // cr�ation de la liste provisoire
  try
    L.Text := ToStr; // on affecte la liste en cours
    for I := 1 to Random(L.Count * 4) do
      L.Exchange(Random(L.Count), Random(L.Count));// on �change
    Result := L.ToStr; // on renvoie le r�sultat
  finally
    L.Free;
  end;
end;

function TGVList.SortItems: string;
// *** tri de la liste ***
var
  L: TGVList;
begin
  L := TGVList.Create; // cr�ation de la liste provisoire
  try
    L.Text := ToStr; // on affecte la liste en cours
    L.Sort; // on trie
    Result := L.ToStr; // on renvoie le r�sultat
  finally
    L.Free; // lib�ration de la liste provisoire
  end;
end;

function TGVList.ReverseItems: string;
// *** inversion de la liste ***
var
  S: string;
begin
  Result := EmptyStr; // cha�ne vide
  try
    for S in Self do
      Result := S + CBlank + Result; // liste � l'envers
  finally
    Result := CBeginList + Trim(Result) + CEndList; // on termine par le d�but !
  end;
end;

function TGVList.ToWBStr: string;
// *** renvoie la liste sous forme de cha�ne sans crochets ***
var
  S: string;
begin
  BeginUpdate;
  Result := EmptyStr; // cha�ne vide par d�faut
  try
    for S in Self do // on construit la liste sans les crochets
      Result := Result + CBlank + S; // �l�ment par �l�ment
  finally
    Result := Trim(Result); // on nettoie les blancs superflus
    EndUpdate;
  end;
end;

function TGVList.ToStr: string;
// *** renvoie la liste sous forme de cha�ne ***
begin
  Result := CBeginList;
  try
    Result := Result + ToWBStr; // on construit la liste
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.TwoAdd(const St1, St2: string): string;
// *** ajoute deux valeurs � la liste ***
// => utile pour les listes de propri�t�s
var
  S: string;
begin
  Result := CBeginList;
  try
    fUtil.TestValue(St1); // liste ou mot valides 1
    fUtil.TestValue(St2); // liste ou mot valides 2
    S := ToWBStr;
    if S = EmptyStr then
      Result := Result + St1 + CBlank + St2
    else
      // on ajoute � la fin
      Result := Result + S + CBlank + St1 + CBlank + St2;
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.TwoDelete(N: Integer): string;
// *** enl�ve deux valeurs � la liste � partir de N ***
// => utile pour les listes de propri�t�s
var
  I: Integer;
begin
  Result := CBeginList; // d�but de la liste
  try
    if (N > (Count - 1)) or (N < 1) then // les �l�ments existent-ils ?
      raise EGVListException.CreateFmt(ME_TwoDelete, [N]) // erreur
    else
    begin
      for I := 1 to Count do // on reconstruit la liste
        if (I <> N) and (I <> N + 1) then // sans les deux �l�ments
          Result := Result + Get(I - 1) + CBlank; // ajout
    end;
  finally
    Result := TrimRight(Result) + CEndList;
  end;
end;

function TGVList.UpperCase: string;
// *** liste en majuscules ***
begin
{$IFDEF Delphi}
  Result := SysUtils.AnsiUpperCase(ToStr);
{$ELSE}
  Result := UTF8UpperCase(ToStr);
{$ENDIF}
end;

{ ========================================================== }

{ TGVListUtils }

constructor TGVListUtils.Create;
// *** cr�ation ***
begin
  inherited Create; // on h�rite
  fWord := TGVWord.Create;  // mot de travail
  fSt := TGVString.Create; // cha�ne de travail
end;

destructor TGVListUtils.Destroy;
// *** destruction ***
begin
  fWord.Free; // on lib�re le mot de travail
  fSt.Free; // idem pour la cha�ne de travail
  inherited Destroy; // on h�rite
end;

function TGVListUtils.EmptyList: string;
// *** renvoie la liste vide ***
begin
  Result := CEmptyList;
end;

function TGVListUtils.IsSimpleList(const St: string): Boolean;
// *** est-ce une liste simple ? ***
begin
  Result := (St <> EmptyStr) and (St[1] = CBeginList) and
    (St[Length(St)] = CEndList);
end;

function TGVListUtils.IsValid(const St: string): Boolean;
// *** teste la validit� d'une liste ***
var
  StTemp: string; // liste de travail
  I: Integer; // caract�re en cours
  Lev: Integer; // niveau interne
begin
  // on cherche les crochets
  Result := (St <> EmptyStr) and (St[1] = CBeginList) and
    (St[Length(St)] = CEndList);
  if not Result then
    Exit; // on sort si d�j� une erreur
      StTemp := Copy(St, 2, Length(St) - 2); // on retire les crochets
  I := 1; // on pointe sur le premier caract�re
  // on boucle tant qu'il n'y a pas d'erreur et qu'il reste des caract�res
  while Result and (I <= Length(StTemp)) do
  begin
    case StTemp[I] of
      CBeginList: // *** d�but d'une sous-liste ? ***
        begin
          Inc(I); // caract�re suivant
          Lev := 1; // premier niveau
          while (Lev <> 0) and (I <= Length(StTemp)) do // autres niveaux ?
          begin
            case StTemp[I] of
              CLink: // un lien ?
                Inc(I); // on le saute
              CBeginList:
                Inc(Lev); // niveau suivant
              CEndList:
                Dec(Lev); // niveau pr�c�dent
            end;
            Inc(I); // caract�re suivant
          end; // fin des autres niveaux
          Result := (Lev = 0); // OK si niveau = 0
        end;
      CBeginPar: // *** d�but d'une expression ? ***
        begin
          Inc(I); // prochaine caract�re
          Lev := 1; // premier niveau
          while Result and (Lev <> 0) and (I <= Length(StTemp)) do
          // autres niveaux sans erreur ?
          begin
            case StTemp[I] of
              CLink: // un lien ?
                Inc(I); // caract�re suivant
              CBeginList, CEndList:
                Result := False; // pas de liste dans une expression
              CBeginPar:
                Inc(Lev); // niveau suivant
              CEndPar:
                Dec(Lev); // niveau pr�c�dent
            end;
            Inc(I); // caract�re suivant
          end; // fin des autres niveaux
          if Result then
            Result := (Lev = 0); // OK si niveau = 0 et pas d'erreur en amont
        end;
      CLink: // *** un lien ? ***
        Inc(I, 2); // on saute un caract�re
      CEndList, CEndPar: // fin de liste ou de parenth�se ? => erreur
        Result := False; // mauvaise liste
    else // autres caract�res
      Inc(I); // on les ignore
    end;
  end; // fin de la boucle
end;


function TGVListUtils.IsValidValue(const St: string): Boolean;
// *** v�rifie que la valeur est soit une liste soit un mot corrects ***
begin
  Result := fWord.IsValid(St) or IsValid(St);
end;

function TGVListUtils.ListToStr(const St: string): string;
// *** change une liste en cha�ne ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  if IsValid(St) then // liste valide ?
    Result := Copy(St, 2, Length(St) - 2) // on enl�ve les crochets
  else
    raise EGVListUtilsException.CreateFmt(ME_BadList, [St]); // erreur
end;

function TGVListUtils.ListToWord(const St: string): string;
// *** change une liste en un mot normalis� ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  fSt.Str := ListToStr(St); // normalise la conversion de la liste
  Result := fSt.Str; // r�sultat r�cup�r�
end;

function TGVListUtils.StrToList(const St: string): string;
// *** transforme une cha�ne en liste ***
begin
  Result := CBeginList + St + CEndList; // on ajoute les crochets
  if not IsValid(Result) then // liste non valide ?
    raise EGVListUtilsException.CreateFmt(ME_BadList, [Result]); // erreur
end;

procedure TGVListUtils.TestValue(const St: string);
// *** test d'une valeur avec exception ***
begin
  if not IsValidValue(St) then // mot ou liste ?
    raise EGVListUtilsException.CreateFmt(ME_NoListWord, [St]); // sinon erreur
end;

function TGVListUtils.WordToList(const St: string): string;
// *** change un mot en liste ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  fSt.Str := St; // normalise la conversion de la liste
  Result := StrToList(fSt.RawStr); // r�sultat r�cup�r� sans formatage
end;

end.
