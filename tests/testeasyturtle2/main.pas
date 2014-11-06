{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo micro-logiciel EASYTURTLE             |
  |                  Description : fiche principale                        |
  |                  Unité : Main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    02-09-2014 12:29:48                          |
  |                  Version : 1.0.1                                       |
  |                                                                        |
  |========================================================================| }

// EasyTurtle - part of GVLOGO
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
//
// 1.0.1 correction : le bouton quitter n'est actif qu'en cas d'enregistrement
// 1.0.0 version initiale

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ActnList, ComCtrls, Buttons, StdCtrls, GVConsts, GVTurtles2,
  GVAbout, Help, GVTools;

const
  // valeurs par défaut
  C_Forward = 15;
  C_BackWard = -15;
  C_Left = 10;
  C_Right = -10;
  C_Length = 50;
  C_MinCmds = 7; // réservation de l'entête

  // ordres 
  // négatifs pour un traitement plus facile :
  // les ordres sont négatifs, les données positives
  CT_Forward = -16;
  CT_Backward = -1;
  CT_Left = -2;
  CT_Right = -3;
  CT_Bigger = -4;
  CT_Minus = -5;
  CT_Erase = -6;
  CT_Home = -7;
  CT_UpDown = -8;
  CT_SeeSaw = -9;
  CT_Kind = -10;
  CT_Pen = -11;
  CT_Back = -12;
  CT_Square = -13;
  CT_Circle = -14;
  CT_Repeat = -15; // réservé
  CT_Unknown = -16; // réservé
  CT_Version = 100; // version actuelle

  // valeurs de la barre de statut
  CSB_Hint = 0;
  CSB_Saved = 1;
  CSB_Turtle = 2;
  CSB_State = 3;

type
  { état du travail de la tortue }

  TStateTurtle = (stLoading, stSaving, stRecording, stPlaying);

  { TMainForm }

  TMainForm = class(TForm)
    ActionStop: TAction;
    ActionUndo: TAction;
    ActionQuit: TAction;
    ActionCircle: TAction;
    ActionSquare: TAction;
    ActionHelp: TAction;
    ActionAbout: TAction;
    ActionTools: TAction;
    ActionLoad: TAction;
    ActionSave: TAction;
    ActionRAZ: TAction;
    ActionReplay: TAction;
    ActionBckColor: TAction;
    ActionPenColor: TAction;
    ActionKind: TAction;
    ActionSeeSaw: TAction;
    ActionUpDown: TAction;
    ActionHome: TAction;
    ActionErase: TAction;
    ActionSmaller: TAction;
    ActionBigger: TAction;
    ActionTurnRight: TAction;
    ActionTurnLeft: TAction;
    ActionBackward: TAction;
    ActionForward: TAction;
    ActionList: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    btnLoad: TBitBtn;
    btnSave: TBitBtn;
    btnRAZ: TBitBtn;
    btnUndo: TBitBtn;
    btnCircle: TBitBtn;
    btnSquare: TBitBtn;
    btnPenColor: TBitBtn;
    btnBckColor: TBitBtn;
    btnKind: TBitBtn;
    btnSeeSaw: TBitBtn;
    btnUpDown: TBitBtn;
    btnHome: TBitBtn;
    btnErase: TBitBtn;
    btnTurtleMinus: TBitBtn;
    btnBigger: TBitBtn;
    btnForward: TBitBtn;
    btnBackward: TBitBtn;
    btnTurnLeft: TBitBtn;
    btnTurnRigth: TBitBtn;
    ColorDialogBack: TColorDialog;
    ColorDialogPen: TColorDialog;
    gbStatements: TGroupBox;
    ImageTurtle: TImage;
    ImgRound: TImage;
    ImageListBigWait: TImageList;
    ImageListWait: TImageList;
    ImageListTurtles: TImageList;
    ImageListCommands: TImageList;
    OpenDialog: TOpenDialog;
    pnlWheel: TPanel;
    pnlColor: TPanel;
    pnlTurtle: TPanel;
    pnlActions: TPanel;
    gbTurtle: TRadioGroup;
    gbColors: TRadioGroup;
    SaveDialog: TSaveDialog;
    ShapeBack: TShape;
    ShapePen: TShape;
    sbStop: TSpeedButton;
    sbReplay: TSpeedButton;
    StatusBar: TStatusBar;
    tbStop: TToolButton;
    ToolBar: TToolBar;
    tbtnForward: TToolButton;
    tbBackward: TToolButton;
    tbLeft: TToolButton;
    tbRight: TToolButton;
    ToolButton1: TToolButton;
    tbErase: TToolButton;
    tbHome: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    tbBigger: TToolButton;
    tbSmaller: TToolButton;
    tbUpDown: TToolButton;
    tbSeeSaw: TToolButton;
    tbKind: TToolButton;
    TbPen: TToolButton;
    tbBack: TToolButton;
    ToolButton3: TToolButton;
    tbSave: TToolButton;
    tbTools: TToolButton;
    tbHelp: TToolButton;
    tbSquare: TToolButton;
    tbLoad: TToolButton;
    tbAbout: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    tbReplay: TToolButton;
    tbCircle: TToolButton;
    ToolButton9: TToolButton;
    TbSpeed: TTrackBar;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionBackwardExecute(Sender: TObject);
    procedure ActionBckColorExecute(Sender: TObject);
    procedure ActionBiggerExecute(Sender: TObject);
    procedure ActionBiggerUpdate(Sender: TObject);
    procedure ActionCircleExecute(Sender: TObject);
    procedure ActionEraseExecute(Sender: TObject);
    procedure ActionForwardExecute(Sender: TObject);
    procedure ActionForwardUpdate(Sender: TObject);
    procedure ActionHelpExecute(Sender: TObject);
    procedure ActionHomeExecute(Sender: TObject);
    procedure ActionKindExecute(Sender: TObject);
    procedure ActionKindUpdate(Sender: TObject);
    procedure ActionLoadExecute(Sender: TObject);
    procedure ActionPenColorExecute(Sender: TObject);
    procedure ActionQuitExecute(Sender: TObject);
    procedure ActionQuitUpdate(Sender: TObject);
    procedure ActionReplayExecute(Sender: TObject);
    procedure ActionReplayUpdate(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSaveUpdate(Sender: TObject);
    procedure ActionSeeSawExecute(Sender: TObject);
    procedure ActionSeeSawUpdate(Sender: TObject);
    procedure ActionSmallerExecute(Sender: TObject);
    procedure ActionSquareExecute(Sender: TObject);
    procedure ActionRAZExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionStopUpdate(Sender: TObject);
    procedure ActionToolsExecute(Sender: TObject);
    procedure ActionTurnLeftExecute(Sender: TObject);
    procedure ActionTurnRightExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionUndoUpdate(Sender: TObject);
    procedure ActionUpDownExecute(Sender: TObject);
    procedure ActionUpDownUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TbSpeedChange(Sender: TObject);
  private
    { private declarations }
    fCmd: Integer; // commande en cours
    fSaved: Boolean; // drapeau d'enregistrement
    fState: TStateTurtle; // état de travail de la tortue
    fForward: Integer; // avance
    fBackWard: Integer; // recule
    fLeft: Integer;  // tourne à gauche
    fLength: Integer; // longueur d'un côté ou diamètre
    fRight: Integer; // tourne à droite
    MemoInt: array of Integer; // mémorisation
    procedure SetSaved(AValue: Boolean); // indicateur de sauvegarde
    procedure SetState(AValue: TStateTurtle); // état du logiciel
    // message de la tortue
    procedure TurtleState(Sender: TObject); dynamic;
    // pour peindre la tortue
    procedure TurtleBeforePaint(Sender: TObject; cHeading: Integer); dynamic;
    procedure Refresh; // remet à jour les voyants
    procedure Memorize(const Value: Integer); // mémorisation des actions
    procedure Replay; // rejoue les actions
  protected
    GVTurtle: TGVTurtle; // tortue
  public
    { public declarations }
    property pForward: Integer read fForward write fForward default C_Forward;
    property pBackward: Integer read fBackward write fBackward default C_Backward;
    property pLeft: Integer read fLeft write fLeft default C_Left;
    property pRight: Integer read fRight write fRight default C_Right;
    property pLength: Integer read fLength write fLength default C_Length;
    property pState: TStateTurtle read fState write SetState default stRecording;
    property pSaved: Boolean read fSaved write SetSaved default True;
  end;

var
  MainForm: TMainForm;

resourcestring
  ME_Replace = 'Voulez-vous remplacer %s ?';
  ME_Triangle = 'Triangle';
  ME_Turtle = 'Animal';
  ME_Visible = 'Visible';
  ME_NotVisible = 'Invisible';
  ME_Write = 'Ecris';
  ME_NotWrite = 'N''écris pas';
  ME_Playing = 'Répétition des ordres...';
  ME_Recording = 'Enregistrement en cours...';
  ME_Loading = 'Chargement en cours...';
  ME_Saving = ' Sauvegarde en cours...';
  ME_LoadError = 'Le fichier %s est corrompu ou inaccessible.';
  ME_SaveError = 'Impossible de sauvegarder le fichier %s.';
  ME_XY = 'X : %.3d - Y : %.3d';
  ME_TVisible = ' Visible : ';
  ME_Heading = ' Cap : %.3d';
  ME_PenDown = ' Crayon baissé : ';
  ME_Close = 'Voulez-vous vraiment arrêter ?';
  ME_VersionError = 'Le fichier %s n''est pas de la version en cours (100).';
  ME_NotSaved = 'La séquence en cours n''a pas été enregistrée.' +
      ' Voulez-vous l''enregistrer avant de quitter EASYTURTLE ?';
  ME_Saved = 'OK';
  ME_ToSave ='MOD';

implementation

{$R *.lfm}

{ TMainForm }
uses
  StrUtils; // pour IFTHEN

procedure TMainForm.ActionForwardExecute(Sender: TObject);
// *** la tortue avance ***
begin
  GVTurtle.Move(pForward); // la tortue bouge
  Memorize(CT_Forward); // mémorisation
end;

procedure TMainForm.ActionForwardUpdate(Sender: TObject);
// actions actives/inactives
begin
  // seulement si enregistrement
  (Sender as TAction).Enabled := (pState = stRecording);
end;

procedure TMainForm.ActionHelpExecute(Sender: TObject);
// *** fiche d'aide ***
begin
   Help.HelpForm := THelpForm.Create(Self); // on crée la fiche
  try
    Help.HelpForm.ShowModal; // on l'affiche
  finally
    Help.HelpForm.Free; // on la libère
  end;
end;

procedure TMainForm.ActionHomeExecute(Sender: TObject);
// *** la tortue retourne au centre de l'écran ***
begin
  Memorize(CT_Home); // mémorisation
  Memorize(Round(GVTurtle.Heading)); // on mémorise le cap initial
  Memorize(Round(GVTurtle.CoordX)); // ancien X
  Memorize(Round(GVTurtle.CoordY)); // ancien Y
  GVTurtle.Home; // tortue au centre
end;

procedure TMainForm.ActionKindExecute(Sender: TObject);
// *** changement de type d'image pour la tortue ***
begin
  if GVTurtle.Kind = tkTriangle then // on inverse l'apparence
    GVturtle.Kind  := tkPng  // tortue dessin
  else
    GVTurtle.Kind := tkTriangle; // triangle
  Memorize(CT_Kind); // mémorisation
  Refresh; // mise à jour des voyants
end;

procedure TMainForm.ActionKindUpdate(Sender: TObject);
// *** type de tortue ***
begin
  // on met à jour le type de tortue
  ActionKind.Caption := IfThen(GVTurtle.Kind = tkTriangle, ME_Turtle,
    ME_TRiangle);
  ActionKind.Enabled := (pState = stRecording);
end;

procedure TMainForm.ActionLoadExecute(Sender: TObject);
// ouverture d'un fichier de commandes
var
  F: TextFile;
  Num, I: Integer;
begin
  if OpenDialog.Execute then // boîte de dialogue d'ouverture de fichier
  begin
    try
      AssignFile(F,OpenDialog.FileName); // fichier assigné
      pState := stLoading;  // statut signifié
      try
        Reset(F); // fichier réinitialisé
        readln(F, Num);
        if Num <> CT_Version then
        begin
          MessageDlg(Format(ME_VersionError,[ExtractFileName(SaveDialog.FileName)]),
          mtError, [mbOk], 0); // signale une erreur de version
          Exit; // sortie
        end;
        readln(F, Num); // récupère les données de l'entête
        pForward := Num;
        readln(F, Num);
        pBackward := Num;
        readln(F, Num);
        pLeft := Num;
        readln(F, Num);
        pRight := Num;
        readln(F, Num);
        pLength := Num;
        readln(F, Num);
        GVTurtle.ScreenColor := Num;
        ActionRAZExecute(Sender); // initialisation
        I := -1; // pointeur de travail pour l'affichage
        repeat
          Inc(I); // élément suivant
          // images des roues mises à jour
          tbReplay.ImageIndex := (I mod 31) + 15;
          ImageListBigWait.GetBitmap(I mod 31 ,ImgRound.Picture.Bitmap);
          Application.ProcessMessages; // on permet les messages
          readln(F, Num); // on lit une donnée
          Memorize(Num); // qu'on enregistre
        until EOF(F) ; // jusqu'à la fin du fichier
      except
        MessageDlg(Format(ME_LoadError,[ExtractFileName(SaveDialog.FileName)]),
          mtError, [mbOk], 0); // signale une erreur de lecture
        ActionEraseExecute(Sender); // on remet à zéro
      end;
    finally
      CloseFile(F); // ferme le fichier
      pState := stRecording; // on enregistre de nouveau
      Refresh;  // remet à jour les voyants
    end;
    ActionReplayExecute(Sender); // rejoue immédiatement la séquence
    pSaved := True; // enregistrement déjà fait
  end;
end;

procedure TMainForm.ActionPenColorExecute(Sender: TObject);
// *** couleur du crayon ***
begin
  if ColorDialogPen.Execute then  // exécute le dialogue
  begin
    Memorize(CT_Pen); // mémorisation
    Memorize(GVTurtle.PenColor); // mémorise l'ancienne couleur
    GVTurtle.PenColor := ColorDialogPen.Color; // récupère la couleur
    Memorize(GVTurtle.PenColor); // mémorisation du code de la couleur
  end;
end;

procedure TMainForm.ActionQuitExecute(Sender: TObject);
// *** demande de fermeture ***
begin
  Close; // on ferme la fenêtre principale
end;

procedure TMainForm.ActionQuitUpdate(Sender: TObject);
// *** actif si on enregistre ***
begin
  ActionQuit.Enabled := (pState = stRecording);
end;

procedure TMainForm.ActionReplayExecute(Sender: TObject);
// *** rejoue la séquence de la tortue ***
begin
  pState := stPlaying; // on rejoue la séquence
end;

procedure TMainForm.ActionReplayUpdate(Sender: TObject);
// *** actif si ordres enregistrés ***
begin
  (Sender as TAction).Enabled := (Length(MemoInt) > C_MinCmds) and
    (pState = stRecording); // ordres présents avec l'état d'enregistrement
  if (pState = stPlaying) then // si rejouer est demandé...
    Replay; // ... on rejoue
end;

procedure TMainForm.ActionSaveExecute(Sender: TObject);
// *** sauvegarde des ordres de la tortue ***
var
  F: TextFile;
  OK: Boolean;
  I: Integer;
begin
  OK := False; // abandon par défaut
  repeat
    Ok := SaveDialog.Execute; // dialogue de sauvegarde
    if Ok then
    begin
      // confirmation si le fichier existe
      if FileExists(SaveDialog.FileName) then  // boîte de dialogue si existe
        // demande de remplacement si le fichier existe
        case MessageDlg(Format(ME_Replace,[ExtractFileName(SaveDialog.FileName)]),
               mtConfirmation, mbYesNoCancel,0) of
          mrYes: Ok := True; // on écrase l'ancien fichier
          mrNo: Ok := False; // on recommence
          mrCancel: Exit; // abandon si le fichier existe
        end;
    end
    else
      Exit; // abandon dès la boîte de dialogue
  until Ok;
  try
    AssignFile(F,SaveDialog.FileName); // fichier assigné
    pState := stSaving; // mode sauvegarde indiqué
    try
      Rewrite(F); // on remet à zéro le fichier
      for I := 0 to Length(MemoInt) - 1 do // on balaie les ordres enregistrés
      begin
        // images qui tournent pendant le chargement
        tbReplay.ImageIndex := (I mod 31) + 15;
        ImageListBigWait.GetBitmap(I mod 31 ,ImgRound.Picture.Bitmap);
        Application.ProcessMessages; // on traite les messages
        writeln(F, MemoInt[I]); // on écrit dans le fichier
      end;
      pSaved := True; // sauvegarde effectuée
    except
      // erreur de sauvegarde
      MessageDlg(Format(ME_SaveError,[ExtractFileName(SaveDialog.FileName)]),
        mtError, [mbOk],0);
    end;
  finally
    CloseFile(F); // fermeture du fichier
    pState := stRecording;
    Refresh;  // remet à jour les voyants
  end;
end;

procedure TMainForm.ActionSaveUpdate(Sender: TObject);
// *** bouton sauvegarde ***
begin
  (Sender as TAction).Enabled := (pState = stRecording) and
    (Length(MemoInt) > C_MinCmds);
end;

procedure TMainForm.ActionSeeSawExecute(Sender: TObject);
// *** la tortue est visible ou invisible ***
begin
  GVTurtle.TurtleVisible := not GVTurtle.TurtleVisible; // visibilité inversée
  Memorize(CT_SeeSaw); // mémorisation
end;

procedure TMainForm.ActionSeeSawUpdate(Sender: TObject);
// *** visibilité de la tortue ***
begin
  ActionSeeSaw.Caption := IfThen(GVTurtle.TurtleVisible, ME_NotVisible,
    ME_Visible); // on met à jour les boutons
  ActionSeeSaw.Enabled := (pState = stRecording);
end;

procedure TMainForm.ActionSmallerExecute(Sender: TObject);
// *** la tortue en triangle rapetisse ***
begin
  if GVTurtle.Size > 5 then // pas de taille inférieure à 4
  begin
    GVTurtle.Size := GVTurtle.Size - 2; // taille diminuée de 2 pixels
    Memorize(CT_Minus); // mémorisation
  end;
end;

procedure TMainForm.ActionSquareExecute(Sender: TObject);
// *** dessine un carré ***
begin
  GVTurtle.Square(pLength); // elle dessine
  Memorize(CT_Square); // mémorisation
end;

procedure TMainForm.ActionRAZExecute(Sender: TObject);
// *** remise à zéro de la séquence de la tortue ***
begin
  SetLength(MemoInt,0); // mémorisation réinitialisée
  Memorize(CT_Version); // on mémorise la version
  Memorize(pForward); // pas en avant
  Memorize(pBackward); // pas en arrière
  Memorize(pLeft); // à gauche
  Memorize(pRight); // à droite
  Memorize(pLength); // longueur
  Memorize(GVTurtle.ScreenColor); // couleur d'écran
  GVTurtle.Home; // à l'origine
  GVTurtle.Wipe; // nettoyage de l'écran
  pSaved := True; // sauvegarde faite
end;

procedure TMainForm.ActionStopExecute(Sender: TObject);
// ** bouton stop ***
begin
  pState := stRecording; // on retourne en mode enregistrement
  SetLength(MemoInt, fCmd); // on ajuste le tableau d'enregistrements
  pSaved := False; // drapeau d'enregistrement
end;

procedure TMainForm.ActionStopUpdate(Sender: TObject);
// *** bouton stop ***
begin
  (Sender as TAction).Enabled := (pState = stPlaying); // seulement si l'on rejoue
end;

procedure TMainForm.ActionToolsExecute(Sender: TObject);
// *** boîte de configuration ***
begin
  GVTools.FormTools := TFormTools.Create(Self); // on crée la fiche
  try
    GVTools.FormTools.ShowModal; // on l'affiche
  finally
    GVTools.FormTools.Free; // on la libère
  end;
end;

procedure TMainForm.ActionTurnLeftExecute(Sender: TObject);
// *** la tortue tourne à gauche ***
begin
  GVTurtle.Turn(pLeft); // elle tourne
  Memorize(CT_Left); // mémorisation
end;

procedure TMainForm.ActionTurnRightExecute(Sender: TObject);
// *** la tortue tourne à droite ***
begin
  GVTurtle.Turn(pRight); // elle tourne
  Memorize(CT_Right); // mémorisation
end;

procedure TMainForm.ActionUndoExecute(Sender: TObject);
// *** défaire la dernière action ***
begin
  if (Length(MemoInt) > (C_MinCmds + 4)) and // ordre le plus long = HOME
    (MemoInt[Length(MemoInt) - 4] = CT_Home) then
  begin
    GVTurtle.PenRubber := True;  // on efface
    GVTurtle.SetPos(Round(MemoInt[Length(MemoInt) - 2])
        , Round(MemoInt[Length(MemoInt) - 1])); // on se repositionne
    GVTurtle.Heading := MemoInt[Length(MemoInt) - 3]; // direction de la tortue
    GVTurtle.PenRubber := False; // on écrit normalement
    SetLength(MemoInt, Length(MemoInt) - 4); // on réajuste la mémorisation
  end
  else
  if (Length(MemoInt) > (C_MinCmds + 3)) and (MemoInt[Length(MemoInt) - 3]
    = CT_Pen) then // couleur de crayon
  begin
    GVTurtle.PenColor := MemoInt[Length(MemoInt) - 2]; // couleur récupérée
    SetLength(MemoInt, Length(MemoInt) - 3); // on ajuste
  end
  else
  begin
    case MemoInt[Length(MemoInt) - 1] of
      CT_Forward: begin  // on a avancé
        GVTurtle.PenRubber := True; // on efface
        GVTurtle.Move(-pForward); // donc on recule
        GVTurtle.PenRubber := False; // on écrit normalement
      end;
      CT_Backward: begin // on a reculé
        GVTurtle.PenRubber := True; // on efface
        GVTurtle.Move(-pBackward); // donc on avance
        GVTurtle.PenRubber := False; // on écrit normalement
      end;
      CT_Left: GVTurtle.Turn(-pLeft); // gauche devient droite
      CT_Right: GVTurtle.Turn(-pRight); // droite devient gauche
      CT_SeeSaw: GVTurtle.TurtleVisible := not GVTurtle.TurtleVisible; // visibilité
      CT_UpDown: GVTurtle.PenDown:= not GVTurtle.PenDown; // écriture ou non
      CT_Kind: if GVTurtle.Kind = tkTriangle then // type de tortue
        GVTurtle.Kind := tkPNG
      else
        GVTurtle.Kind := tkTriangle;
      CT_Bigger: GVTurtle.Size := GVTurtle.Size - 2; // on a grossi
      CT_Minus: GVTurtle.Size := GVTurtle.Size + 2; // on a rapetissé
      CT_Circle: begin  // un cercle
        GVTurtle.PenRubber:= True;
        GVTurtle.Circle(pLength+1);
        GVTurtle.PenRubber:= False;
      end;
      CT_Square: begin  // un carré
        GVTurtle.PenRubber:= True;
        GVTurtle.Square(pLength+1);
        GVTurtle.PenRubber:= False;
      end;
    end;
    SetLength(MemoInt, Length(MemoInt) - 1); // on ajuste la mémorisation
  end;
  pSaved := not (Length(MemoInt) > C_MinCmds); // drapeau d'enregistrement
end;

procedure TMainForm.ActionUndoUpdate(Sender: TObject);
// mise à jour du bouton "défaire"
begin
  // seulement s'il n'y a pas que l'entête et si on enregistre
  ActionUndo.Enabled := (Length(MemoInt) > C_MinCmds) and (pState = stRecording);
end;

procedure TMainForm.ActionUpDownExecute(Sender: TObject);
// *** le crayon écrit ou non ***
begin
  GVTurtle.PenDown := not GVTurtle.PenDown; // écriture inversée
  Memorize(CT_UpDown); // mémorisation
end;

procedure TMainForm.ActionUpDownUpdate(Sender: TObject);
// *** crayon levé ou non ***
begin
  // mise à jour de l'intitulé du bouton
  ActionUpDown.Caption := IfThen(GVTurtle.PenDown, ME_NotWrite, ME_Write);
  ActionUpDown.Enabled := (pState = stRecording); // activé si enregistrement
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
// *** demande de fermeture ***
begin
  // fermeture à confirmer
  if not pSaved then // si séquence non enregistrée
  begin  // demande d'enregistrement
    case MessageDlg(ME_NotSaved, mtConfirmation, mbYesNoCancel,0) of
      mrYes: begin // oui
        ActionSaveExecute(Sender); // sauvegarde
        CanClose := pSaved; // on sort si c'est fait
      end;
      mrNo: CanClose := True; // on sort
      mrCancel: CanClose := False; // on ne sort pas
    end;
  end
  else // cas où il n'y a rien à enregistrer
    CanClose := (MessageDlg(ME_Close, mtConfirmation, mbYesNo,0) = mrYes);
  // on arrête le dessin si nécessaire
 if CanClose then
   pState := stRecording;
end;

procedure TMainForm.FormCreate(Sender: TObject);
// *** création de la fenêtre ***
begin
  // valeurs par défaut des propriétés
  fForward := C_Forward;
  fBackward := C_Backward;
  fLeft := C_Left;
  fRight := C_Right;
  fLength := C_Length;
  fState := stRecording;
  GVTurtle := TGVTurtle.Create(ImageTurtle.Width, ImageTurtle.Height); // dans le panneau prévu
  GVTurtle.ReInit; // initialisation
  GVTurtle.OnChange := @TurtleState;  // gestionnaire de changement
  GVTurtle.OnBeforeChange := @TurtleBeforePaint; // idem avant de dessiner
  GVTurtle.Screen := teGate; // limite l'écran
  // affiche les couleurs en cours
  ShapePen.Brush.Color := GVTurtle.PenColor;
  ShapeBack.Brush.Color := GVTurtle.ScreenColor;
  ActionEraseExecute(Sender); // initialisations
  pState := stRecording; // on enregistre
  pSaved := True; // séquence enregistrée
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// *** destruction de la fenêtre ***
begin
  GVTurtle.Free; // on libère la tortue
end;

procedure TMainForm.TbSpeedChange(Sender: TObject);
// *** vitesse de la tortue ***
begin
  GVTurtle.Speed := tbSpeed.Position;
end;

procedure TMainForm.TurtleState(Sender: TObject);
// *** état de la tortue ***
begin
  GVTurtle.TurtleBitmap.Draw(ImageTurtle.Canvas,0,0);
  ImageTurtle.Invalidate;
  // données de la tortue sur la barre de statut
 with GVTurtle do
   Statusbar.Panels[CSB_Turtle].Text := Format('X: %.3d Y: %.3d Cap: %.3d',
      [Round(CoordX), Round(CoordY), Round(Heading)]) +
      ' Visible: ' + IfThen(TurtleVisible, P_True, P_False) +
      ' Baissé: ' + IfThen(PenDown, P_True, P_False);
 // on met à jour les indicateurs de couleur
 ShapePen.Brush.Color := GVTurtle.PenColor;
 ShapeBack.Brush.Color := GVTurtle.ScreenColor;
end;

procedure TMainForm.SetState(AValue: TStateTurtle);
// *** état modifié ***
begin
  if fState = AValue then // sortie si inchangé
    Exit;
  fState := AValue; // changement du statut
  with Statusbar.Panels[CSB_State] do // mise à jour de la barre de statut
    case fState of
      stRecording: Text := ME_Recording; // on enregistre
      stPlaying: Text := ME_Playing; // on joue
      stLoading: Text := ME_Loading; // on charge
      stSaving: Text := ME_Saving; // on sauvegarde
    end;
end;

procedure TMainForm.SetSaved(AValue: Boolean);
// *** sauvegarde indiquée ***
begin
  if fSaved = AValue then
    Exit;
  fSaved := AValue;
  // mise à jour de la barre de statut
  statusbar.Panels[CSB_Saved].Text := IfThen(fSaved, ME_Saved, ME_ToSave);
end;

procedure TMainForm.TurtleBeforePaint(Sender: TObject; cHeading: Integer);
// *** image associée à la tortue ***
var
  BitM: TBitmap;
begin
  // charge l'image de la tortue
  BitM := TBitmap.Create;
  try
    // les images de la tortue sont proposées tous les 5 degrés
    ImageListTurtles.GetBitmap(Round(cHeading) div 5, BitM);
    // celle qui correspond est assignée au bitmap
    GVTurtle.PNGTurtle.Assign(BitM);
  finally
    BitM.Free;
  end;
end;

procedure TMainForm.Refresh;
// *** remet à jour les voyants ***
begin
  with ImgRound.Picture.Bitmap do // on nettoie l'image tournante
    Canvas.FillRect(Canvas.ClipRect);
  tbReplay.ImageIndex := 13; // image de la barre d'outils réinitialisée
end;

procedure TMainForm.Memorize(const Value: Integer);
// *** mémorisation d'une action ***
begin
  SetLength(MemoInt, Length(MemoInt) + 1); // on augmente la taille du tableau
  MemoInt[Length(MemoInt) - 1] := Value; // on enregistre
  pSaved := False; // séquence non enregistrée
end;

procedure TMainForm.Replay;
// *** rejoue les actions ***
begin
  GVTurtle.ReInit; // réinitialisation de la tortue
  GVTurtle.Speed := TbSpeed.Position; // vitesse selon barre
  GVTurtle.Screen := teGate; // écran clos
  fCmd := C_MinCmds; // on pointe sur le premier élément du tableau hors entête
  pForward := MemoInt[1]; // on récupère les données de l'entête
  pBackward := MemoInt[2];
  pLeft := MemoInt[3];
  pRight := MemoInt[4];
  pLength := MemoInt[5];
  GVTurtle.ScreenColor := MemoInt[6];
  // on boucle tant qu'il y a des ordres et qu'un arrêt n'a pas été demandé
  while (fCmd < Length(MemoInt)) and (pState = stPlaying) do  // on balaie le tableau
  begin
     //images adaptées pour les roues
    tbReplay.ImageIndex := (fCmd mod 31) + 15;
    ImageListBigWait.GetBitmap(fCmd mod 31 ,ImgRound.Picture.Bitmap);
    // on permet aux messages d'être traités
    Application.ProcessMessages;
    // on répartit le travail suivant les ordres enregistrés
    case MemoInt[fCmd] of
      CT_Forward : GVturtle.Move(pForward); // avance
      CT_Backward : GVturtle.Move(pBackward); // recule
      CT_Left : GVturtle.Turn(pLeft); // à gauche
      CT_Right : GVturtle.Turn(pRight); // à droite
      CT_Bigger : GVturtle.Size := GVTurtle.Size + 2;  // taille + 2
      CT_Minus : GVturtle.Size := GVTurtle.Size - 2; // taille - 2
      CT_Home :  begin
        GVTurtle.Home; // maison
        Inc(fCmd, 3); // ordre suivant
      end;
      CT_UpDown : GVTurtle.PenDown := not GVTurtle.PenDown; // crayon baissé
      // visibilité
      CT_SeeSaw : GVTurtle.TurtleVisible := not GVTurtle.TurtleVisible;
      CT_Kind : if GVTurtle.Kind <> tkTriangle then // type
                  GVTurtle.Kind := tkTriangle
                else
                  GVTurtle.Kind := tkPng;
      CT_Pen : begin  // couleur crayon
        Inc(fCmd,2); // ordre suivant
        GVTurtle.PenColor := MemoInt[fCmd];
      end;
      CT_Square: GVTurtle.Square(pLength); // carré
      CT_Circle: GVTurtle.Circle(pLength); // cercle
    end;
    Inc(fCmd); // ordre suivant
  end;
  Refresh; // remet à jour les voyants
  pState := stRecording; // on repasse en mode enregistrement
end;

procedure TMainForm.ActionBackwardExecute(Sender: TObject);
// *** la tortue recule ***
begin
  GVTurtle.Move(pBackward); // avance négative
  Memorize(CT_Backward); // mémorisation
end;

procedure TMainForm.ActionAboutExecute(Sender: TObject);
// *** boîte à propos ***
begin
  GVAbout.AboutForm := TAboutForm.Create(Self); // on crée la fiche
  try
    GVAbout.AboutForm.ShowModal; // on l'affiche
  finally
    GVAbout.AboutForm.Free; // on la libère
  end;
end;

procedure TMainForm.ActionBckColorExecute(Sender: TObject);
// *** changement du fond de l'écran ***
begin
  if ColorDialogBack.Execute then  // exécute le dialogue
    GVTurtle.ScreenColor := ColorDialogBack.Color; // récupère la couleur
end;

procedure TMainForm.ActionBiggerExecute(Sender: TObject);
// *** la tortue en triangle grossit ***
begin
  if GVTurtle.Size < (CMaxSize - 1) then // pas de taille supérieure à 20
  begin
    GVTurtle.Size := GVTurtle.Size + 2;  // on augmente la taille
    Memorize(CT_Bigger); // mémorisation
  end;
end;

procedure TMainForm.ActionBiggerUpdate(Sender: TObject);
// *** la tortue grossit/rapetisse ***
begin
  // seulement si enregistrement et tortue triangulaire
  (Sender as TAction).Enabled := (pState = stRecording)
    and (GVTurtle.Kind = tkTriangle);
end;

procedure TMainForm.ActionCircleExecute(Sender: TObject);
// *** dessine un cercle ***
begin
  GVTurtle.Circle(pLength); // elle dessine
  Memorize(CT_Circle); // mémorisation
end;

procedure TMainForm.ActionEraseExecute(Sender: TObject);
// *** la surface de dessin est effacée ***
begin
  GVTurtle.ScreenColor := clBlack;  // on efface : écran noir
  gvTurtle.PenColor := clWhite; // écriture blanche
  GVTurtle.Screen := teGate; // écran clos
  ActionRAZExecute(Sender); // on réinitialise le tableau des ordres
end;

end.

