{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Automate d'interprétation               |
  |                  Unité : GVAutomat.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVAUTOMAT - part of GVLOGO
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

unit GVAutomat;
// Cette unité contient l'automate d'interprétation
// du logiciel GVLOGO.

 interface

uses
  Classes, SysUtils,  Forms,
  GVConsts, // constantes communes
  GVEval, // évaluation
  GVKernel, // espace de travail
  GVTurtles, // tortue
  GVWords, // mots
  GVErrConsts, // constantes d'erreurs
  GVPrimConsts, // constantes de primitives
  GVErrors, // erreurs
  GVLists, // listes
  GVLocVars, // variables locales
  GVStacks; // piles

type
  // *** message de l'automate ***

  { TGVAutomatMessage }

  TGVAutomatMessage = class(TObject)
    strict private
      fCmd: TGVAutomatCmd; // commande en cours
      fMessage: string; // message associé
      fOnChange: TNotifyEvent;
      procedure SetCmd(AValue: TGVAutomatCmd);
      procedure SetMessage(AValue: string);
    protected
      procedure Change; // changement notifié
    public
      constructor Create; // création
      destructor Destroy; override; // destructeur
      procedure Clear; // nettoyage
      // données regroupées
      procedure SetMessageAll(const ACmd: TGVAutomatCmd; AMess: string);
      // message
      property Message: string read fMessage write SetMessage;
      //commande
      property Cmd: TGVAutomatCmd read fCmd write SetCmd default acNone;
      // gestionnaire de changement
      property OnMessageChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  // *** automate ***
  TGVAutomat = class(TObject)
    strict private
      fError: TGVErrors; // traitement des erreurs
      fMessage: TGVAutomatMessage; // message de l'interpréteur
      fFollow: Boolean; // drapeau de suivi
      fOnStateChange: TNotifyEvent; // événement d'état
      fState: TGVAutomatState; // état de l'automate
      fWkRec: TGVAutomatRec; // espace d'interprétation
      fReturnFlag: Boolean; // drapeau de retour
      fReturnVal: string; // valeur de retour
      fTurtleOutput: Boolean; // sortie vers l'écran de la tortue
      fParamsStack: TGVIntegerStack; // pile des paramètres
      fDatasStack: TGVStringStack; // pile des données
      fCommandsStack: TGVStringStack; // pile des commandes
      fExeStack: TGVStringStack; // pile d'exécution
      fWkStack: TGVStringStack; // pile de travail
      fStop: Boolean; // drapeau d'arrêt
      fElse: TThreeStates; // drapeau de Sinon
      fTest: TThreeStates; // drapeau de Test
      fKernel: TGVLogoKernel; // espace de travail
      fEval: TGVEval; // évaluateur
      fTurtle: TGVTurtle; // tortue
      fLocVars: TGVLocVars; // variables locales
      procedure SetFollow(AValue: Boolean);
      procedure SetState(AValue: TGVAutomatState); // définition de l'état
      procedure SetStop(AValue: Boolean); // stop ?
      procedure PushConst(const St: string); // empilement d'une constante
      function DoBegin(const St: string): Boolean; // préparation
      // préparation de l'interpréteur
      procedure DoEnd; // postparation de l'interpréteur
      procedure DoWord; // traitement d'un mot
      procedure DoList; // traitement d'une liste
      procedure DoNumber; // traitement d'un nombre
      procedure DoVar; // traitement d'une variable
      procedure DoEval; // traitement d'une expression
      procedure DoCommand; // traitement d'une commande
      procedure DoPrim; // traitement d'une primitive
      procedure DoProc; // traitement d'une procédure
      procedure ExePrim; // exécution d'une primitive
      procedure ExeProc; // exécution d'une procédure
      procedure ExeCommand; // exécution d'une commande
    protected
      procedure StateChange; // changement d'état notifié
    public
      constructor Create; // constructeur
      destructor Destroy; override; // destructeur
      procedure Clear; // nettoyage
      procedure ClearAll; // nettoyage complet
      procedure Process(const St: string); // lancement de l'automate
      procedure SetError(const Code: TGVError; ErrItem: string;
        ErrPos: Integer = CE_NoErr); // gestion des erreurs
      // noyau de travail
      property Kernel: TGVLogoKernel read fKernel write fKernel;
      // évaluateur
      property Eval: TGVEval read fEval write fEval;
      property Turtle: TGVTurtle read fTurtle write fTurtle; // tortue
      // variables locales
      property LocVars: TGVLocVars read fLocVars write fLocVars;
      // messages
      property Message: TGVAutomatMessage read fMessage write fMessage;
      // erreur
      property Error: TGVErrors read fError write fError;
      // état de l'automate
      property State: TGVAutomatState read fState write SetState
        default asWaiting;
      // notification de changement d'état
      property OnStateChange: TNotifyEvent read fOnStateChange
        write fOnStateChange;
      property Datas: TGVAutomatRec read fWkRec; // données
      // trace de l'exécution
      property Follow: Boolean read fFollow write SetFollow default False;
      // arrêt
      property Stop: Boolean read fStop write SetStop default False;
  end;

implementation

uses
  {%H-}StrUtils, // directive nécessaire pour IFTHEN (voir TGVWORDS)
  Math;

{ TGVAutomatMessage }

procedure TGVAutomatMessage.SetCmd(AValue: TGVAutomatCmd);
// *** commande pour l'automate ***
begin
  fCmd := AValue; // commande mémorisée
  Change; // changement notifié
end;

procedure TGVAutomatMessage.SetMessage(AValue: string);
// *** message pour l'automate ***
begin
  if fMessage = AValue then // pas de changement ?
    Exit; // on sort
  fMessage := AValue; // nouveau message affecté
end;

procedure TGVAutomatMessage.Change;
// *** changement d'un message en attente ***
begin
  if Assigned(fOnChange) then // gestionnaire affecté ?
    fOnChange(Self); // on l'exécute
end;

constructor TGVAutomatMessage.Create;
// *** création de l'objet ***
begin
  Clear; // on nettoie
end;

destructor TGVAutomatMessage.Destroy;
// *** destruction de l'objet ***
begin
  inherited Destroy; // on hérite
end;

procedure TGVAutomatMessage.Clear;
// *** nettoyage de l'objet ***
begin
  SetMessageAll(acNone, EmptyStr); // message à zéro
end;

procedure TGVAutomatMessage.SetMessageAll(const ACmd: TGVAutomatCmd;
  AMess: string);
// *** message complet ***
begin
  Message := AMess; // message affecté
  Cmd := ACmd; // commande affectée et effectuée
end;

{ TGVAutomat }

procedure TGVAutomat.SetStop(AValue: Boolean);
// *** gestion du drapeau d'arrêt ***
begin
  if fStop = AValue then // pas de changement ?
    Exit; // on sort
  fStop := AValue; // nouvelle valeur de l'arrêt
  if fStop then
    State := asStopped; // état
end;

procedure TGVAutomat.SetState(AValue: TGVAutomatState);
// *** état en cours ***
begin
  if fState = AValue then // pas de changement ?
    Exit; // on sort
  fState := AValue; // nouvel état
  StateChange; // changement notifié
end;

procedure TGVAutomat.SetFollow(AValue: Boolean);
// *** trace de l'exécution ***
begin
  if fFollow = AValue then // pas de changement ?
    Exit; // on sort
  fFollow := AValue; // nouvelle valeur de la trace
  if fFollow then
    State := asFollowing  // état adapté
  else
    State := asEndFollowing;
end;

function TGVAutomat.DoBegin(const St: string): Boolean;
// *** préparation de l'automate ***
var
  LL : TGVList;
  Li: Integer;
begin
  fWkRec.fItem := St; // élément analysé
  State := asPreparing; // état
  Result := False; // suppose une erreur
  if St = EmptyStr then // rien à traiter
    Exit; // on sort
  LL := TGVList.Create; // liste de travail créée
  try
    LL.Text := St; // affectation du texte à la liste
    if LL.IsValid then  // liste OK ?
    begin
      fWkStack.Push(CBreak); // marque de fin
      for Li := LL.Count downto 1 do // on empile à l'envers
        fWkStack.Push(LL[Li - 1]);
      fWkRec.fLine := St; // on conserve la ligne à analyser
      fWkRec.fNum := 0; // élément dans la ligne
      fElse := CDisabledState; // sinon désactivé
      fTest := CDisabledState; // test aussi
      Result := True; // tout est OK
    end
    else
      // [### Erreur: liste incorrecte ###]
      SetError(CE_BadList, St);
  finally
    LL.Free; // liste de travail libérée
  end;
end;

procedure TGVAutomat.DoEnd;
// *** postparation de l'automate ***
begin
  State := asEnding; // état
  // commandes en attente sans drapeau de valeur rendue, sans erreur et
  // sans arrêt ?
  if (not fReturnFlag) and (fCommandsStack.Count <> 0)
    and Error.Ok and (not Stop) then
  begin
    fWkRec.fItem := fCommandsStack.Pop; // on récupère la commande en suspens
    if fWkRec.fItem[1] = CLink then // une primitive ?
    begin
      fWkRec.fItem := Copy(fWkRec.fItem, 2, CMaxLengthPrim); // $ supprimé
      fWkRec.fPrim := fWkRec.fItem; // une primitive
    end
    else
      fWkRec.fProc := fWkRec.fItem; // une procédure
    // [### Erreur: pas assez de données ###]
    SetError(CE_NotEnoughDatas, fWkRec.fItem);
  end;
end;

procedure TGVAutomat.ExePrim;
// *** exécution d'une primitive ***
var
  Li, LPrm: Integer;
  LW: TGVWord;
  LL: TGVList;
  LU: TGVListUtils;

  {$I Prims.inc}  // liste des méthodes pour les primitives

begin
  // nom de la primitive (sans le $)
  fWkRec.fPrim := Copy(fCommandsStack.Pop, 2, CMaxLengthPrim);
  State := asExePrim; // état
  try
    Li := fKernel.NumPrim(fWkRec.fPrim); // numéro retrouvé
    fParamsStack.Pop; // on libère les paramètres
    LPrm := fParamsStack.Pop; // nombre de paramètres de la primitive récupéré
    while LPrm <> 0 do // tant qu'il y a des paramètres
    begin
      // on empile les paramètres sur la pile d'exécution
      fExeStack.Push(fDatasStack.Pop);
      Dec(LPrm); // paramètre suivant
    end;
    LL := TGVList.Create; // liste de travail
    try
      LW := TGVWord.Create; // mot de travail
      try
        LU := TGVListUtils.Create; // utilitaire de travail
        try
          DoExePrim(Li); // exécution
        finally
          LU.Free; // libération de l'utilitaire de travail
        end;
      finally
        LW.Free; // libération du mot de travail
      end;
    finally
      LL.Free; // libération de la liste de travail
    end;
    Application.ProcessMessages; // permet l'affichage fluide
  finally
    State := asPrimDone; // état
  end;
end;

procedure TGVAutomat.ExeProc;
// *** exécution d'une procédure ***
var
  LPm, Li, Lj: Integer;
  LS: string;
  LL: TGVList;
  LOldProc: string;
begin
  LOldProc := fWkRec.fProc; // on conserve l'ancienne procédure
  Inc(fWkRec.fLevel); // niveau suivant
  fWkRec.fProc := fCommandsStack.Pop; // nom de la procédure en cours
  fWkRec.fItem := fWkRec.fProc; // élément en cours actualisé
  State := asExeProc; // état
  try
    fParamsStack.Pop; // on nettoie le sommet de la pile des paramètres
    LPm := fParamsStack.Pop; // nombre de paramètres récupéré
    fLocVars.AddLocNumber(LPm); // sauvegarde dans pile des variables locales
    // récupération des paramètres et création des variables locales
    if LPm <> 0 then // s'il y a des paramètres
      for Li := LPm downto 1 do // ordre inversé !
        with fKernel do
          // variables locales initialisées
          fLocVars.AddLocVar(ParamNum(fWkRec.fProc, Li), fDatasStack.Pop);
    // récupération des lignes de la procédure
    LL := TGVList.Create;
    try
      LS := EmptyStr; // définition vide
      for Li := 1 to fKernel.ProcLinesCount(fWkRec.fProc) do
      begin
        if not Stop then
        begin
          LL.Text := fKernel.ProcLine(fWkRec.fProc, Li);
          for Lj := 1 to LL.Count do // on balaie la ligne
            if Trim(LL[Lj - 1]) <> CComment then // commentaire ?
              LS := LS + CBlank + LL[Lj - 1]
            else
              Break; // on sort si commentaire
        end;
      end;
      // exécution de la procédure
      Process(CBeginList + LS + CEndList);
    finally
      LL.Free; // libération de la liste
    end;
    if fLocVars.LocVarsCount > 0 then // s'il y a des variables en suspens
      fLocVars.DelLastGroup; // variables locales supprimées
    if fReturnFlag then // valeur en attente ?
    begin
      fReturnFlag := False; // drapeau baissé
      PushConst(fReturnVal); // valeur empilée
    end;
  finally
    State := asProcDone; // état
    Dec(fWkRec.fLevel); // niveau précédent
    fWkRec.fProc := LOldProc; // récupération de l'ancienne procédure
  end;
end;

procedure TGVAutomat.ExeCommand;
// *** exécution d'une commande ***
begin
  if fCommandsStack.Peek[1] = CLink then // $ ?
    ExePrim // c'est une primitive
  else
    ExeProc; // sinon une procédure
end;

procedure TGVAutomat.PushConst(const St: string);
// *** empilement d'une constante ***
begin
  fWkRec.fItem := St;
  State := asPushing; // état
  fDatasStack.Push(St); // on empile la constante
  if fReturnFlag then // si drapeau de valeur retournée
    Exit; // on sort
  // pas de paramètre ou pas de commande en attente ?
  if (fParamsStack.Count = 0) or
    ((fParamsStack.Peek > 0) and (fCommandsStack.Count = 0)) then
  begin
    fWkRec.fPrim := EmptyStr; // pas de primitive en cours
    // [### Erreur : que faire de ? ###]
    SetError(CE_WhatAbout, St);
  end
  else
  begin
    fParamsStack.Push(fParamsStack.Pop - 1); // un paramètre a été trouvé
    if (fParamsStack.Peek = 0) then // plus de paramètres en attente ?
      // on exécute la commande en attente
      ExeCommand;
  end;
end;

procedure TGVAutomat.DoWord;
// *** traitement d'un mot ***
var
  LW: TGVWord;
begin
  State := asWord; // état
  LW := TGVWord.Create; // mot de travail créé
  try
    LW.Text := fWkRec.fItem; // normalisation du mot
    LW.Text := LW.WithoutQuote; // sans le "
    PushConst(LW.FmtText); // constante empilée
  finally
    LW.Free; // mot de travail libéré
  end;
end;

procedure TGVAutomat.DoList;
// *** traitement d'une liste ***
begin
  State := asList; // état
  PushConst(fWkRec.fItem); // constante empilée
end;

procedure TGVAutomat.DoNumber;
// *** traitement d'un nombre ***
var
  LW: TGVWord;
begin
  State := asNumber; // état
  LW := TGVWord.Create; // mot de travail créé
  try
    LW.Text := fWkRec.fItem; // normalisation du mot
    if LW.IsNumber then // un nombre ?
      PushConst(LW.Text) // on empile la constante
    else
      // [### Erreur: nombre invalide ###]
      SetError(CE_BadNumber, LW.Text);
  finally
    LW.Free; // mot de travail libéré
  end;
end;

procedure TGVAutomat.DoVar;
// *** traitement d'une variable ***
var
  LW: TGVWord;
begin
  State := asVar; // état
  LW := TGVWord.Create; // mot de travail créé
  try
    LW.Text := fWkRec.fItem; // nom analysé
    LW.Text := LW.WithoutColon; // : retirés
    if fLocVars.IsLocVar(LW.Text) then // variable locale ?
      PushConst(fLocVars.DirectValLocVar) // on empile sa valeur
    else
    if fKernel.IsVar(LW.Text) then // variable globale ?
      PushConst(fKernel.ValVar(LW.Text)) // on empile sa valeur
    else
      // [### Erreur: variable inconnue ###]
      SetError(CE_UnknownVar, LW.Text);
  finally
    LW.Free; // libération du mot de travail
  end;
end;

procedure TGVAutomat.DoEval;
// *** traitement d'une expression ***
begin
  State := asEval; // état
  fEval.Text := fWkRec.fItem; // on affecte à l'évaluateur
  fEVal.Scan; // on évalue
  if fEval.Error.Ok then  // pas d'erreur ?
    PushConst(FloatToStr(fEval.Res)); // valeur empilée
end;

procedure TGVAutomat.DoPrim;
// *** traitement d'une primitive ***
var
  Li: Integer;
begin
  State := asPrim; // état
  // nombre de paramètres associés
  Li := fKernel.NumParamsPrim(fWkRec.fItem);
  fParamsStack.Push(Li); // on enregistre le nombre d'arguments attendus
  fParamsStack.Dup; // on double ce nombre
  // $ + nom de la primitive empilé
  fCommandsStack.Push(CLink + fWkRec.fItem);
  if Li = 0 then // pas de paramètres ?
    ExePrim; // exécute immédiatement la primitive
end;

procedure TGVAutomat.DoProc;
// *** traitement d'une procédure ***
var
  Li: Integer;
begin
  State := asProc; // état
  // nombre de paramètres associés
  Li := fKernel.ParamsCount(fWkRec.fItem);
  fParamsStack.Push(Li); // on enregistre le nombre d'arguments attendus
  fParamsStack.Dup; // on double ce nombre
  // nom de la procédure empilée
  fCommandsStack.Push(fWkRec.fItem);
  if Li = 0 then // pas de paramètres ?
    ExeProc; // exécute immédiatement la procédure
end;

procedure TGVAutomat.DoCommand;
// *** répartit les commandes ***
begin
  if fKernel.IsPrim(fWkRec.fItem) then // une primitive ?
    DoPrim // on la traite
  else
  if fKernel.IsProc(fWkRec.fItem) then // une procédure ?
    DoProc // on la traite
  else
    // [### Erreur: ni une primitive ni une procédure ###]
    SetError(CE_NorPrimNorProc, fWkRec.fItem);
end;

procedure TGVAutomat.Process(const St: string);
// *** lancement de l'automate d'interprétation ***
begin
  if DoBegin(St) then // préparation correcte ?
  begin
    State := asWorking; // état
    // on boucle si pile non vide, stop non demandé et pas d'erreur
    while not (fWkStack.IsEmpty or Stop or (not Error.Ok)) do
    begin
      fWkRec.fItem := fWkStack.Pop; // élément dépilé
      Inc(fWkRec.fNum); // numéro conservé
      case fWkRec.fItem[1] of // premier caractère déterminant
        CBreak: Break; // <========== fin de travail
        CQuote: DoWord; // <====== littéral
        CBeginList: DoList; // <====== liste
        CColon: DoVar; // <====== variable
        CPlus, CMinus, '0'..'9' : DoNumber; // <====== nombre
        'a'..'z','A'..'Z',CUnderline, CDot : DoCommand; // <====== commande
        CBeginPar: DoEval; // <====== valeur à évaluer
      end;
    end;
    DoEnd; // postparation
  end;
  State := asWaiting; // état
end;

procedure TGVAutomat.StateChange;
// *** notification de changement d'état ***
begin
  if Assigned(fOnStateChange) then // si actif
    fOnStateChange(Self); // on exécute le gestionnaire
end;

constructor TGVAutomat.Create;
// *** création ***
begin
  // piles
  fWkStack := TGVStringStack.Create; // travail
  fParamsStack := TGVIntegerStack.Create; // paramètres
  fDatasStack := TGVStringStack.Create; // données
  fCommandsStack := TGVStringStack.Create; // commandes
  fExeStack := TGVStringStack.Create; // exécution
  // drapeaux
  fStop := False; // pas d'arrêt
  fFollow := False; // pas de suivi
  fTurtleOutPut:= False; // écriture sur l'écran normal
  fState := asWaiting; // en attente
  // modules
  Error := TGVErrors.Create; // traitement des erreurs
  fKernel:= TGVLogoKernel.Create; // noyau
  fKernel.Error.OnError := @Error.GetError; // gestionnaire centralisé d'erreurs
  fLocVars := TGVLocVars.Create; // variables locales
  fLocVars.Error.OnError := @Error.GetError; // gestionnaire centralisé d'erreurs
  fEval := TGVEval.Create; // évaluateur
  fEval.Kernel := fKernel; // noyau et évaluateur liés
  fEval.LocVars := fLocVars; // idem pour les variables locales
  fEval.Error.OnError := @Error.GetError; // gestionnaire centralisé d'erreurs
  fMessage := TGVAutomatMessage.Create; // messages
end;

destructor TGVAutomat.Destroy;
// *** destruction ***
begin
  fWkStack.Free; // pile de travail
  fParamsStack.Free; // pile des paramètres
  fDatasStack.Free; // pile des données
  fCommandsStack.Free; // pile des commandes
  fExeStack.Free; // pile d'exécution
  Error.Free; // erreurs
  fEval.Free; // évaluateur
  fKernel.Free; // noyau
  fLocVars.Free; // variables locales
  fMessage.Free; // messages
  inherited Destroy; // on hérite
end;

procedure TGVAutomat.Clear;
// *** nettoyage ***
begin
  fWkStack.Clear; // pile de travail
  with fWkRec do
  begin
    fNum := 0; // élément en cours dans la ligne
    fItem := EmptyStr; // donnée en cours
    fLine := EmptyStr; // ligne en cours
    fPrim := EmptyStr; // primitive en cours
    fProc := EmptyStr; // procédure en cours
    fLevel := 0; // niveau en cours
  end;
  Message.Clear; // message réinitialisé
  fParamsStack.Clear; // pile des paramètres
  fDatasStack.Clear; // pile des données
  fCommandsStack.Clear; // pile des commandes
  fExeStack.Clear; // pile d'exécution
  Stop := False; // pas d'arrêt
  Error.Clear; // erreurs
  fEval.Clear; // évaluateur
  fLocVars.Clear; // variables locales
  fReturnFlag := False; // pas de retour
  Follow := False; // pas de suivi
  fTurtleOutPut:= False; // écriture sur l'écran normal
  State := asWaiting; // état
end;

procedure TGVAutomat.ClearAll;
// *** nettoyage complet ***
begin
  fKernel.Clear; // noyau nettoyé
  Clear; // et le reste aussi
end;

procedure TGVAutomat.SetError(const Code: TGVError; ErrItem: string;
  ErrPos: Integer);
// *** gestion des erreurs ***
begin
  State := asError; // état
  Error.SetError(Code, ErrItem, ErrPos); // erreur transmise
end;

end.

