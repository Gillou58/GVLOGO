{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test de l'unité TGVStacks               |
  |                  Unité : TestPiles.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    08-08-2014 17:17:49                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }
unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Grids, ComCtrls, GVStacks, GVConsts;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnPush: TBitBtn;
    btnPop: TBitBtn;
    btnPeek: TBitBtn;
    btnSwap: TBitBtn;
    btnDup: TBitBtn;
    btnRot: TBitBtn;
    btnOver: TBitBtn;
    btnEqualP: TBitBtn;
    btnClear: TBitBtn;
    BitBtnClose: TBitBtn;
    lblCapacity: TLabel;
    lblDepth: TLabel;
    mmoActions: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    sbStack: TStatusBar;
    sgStack: TStringGrid;
    procedure btnPushClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FStackStr : TGVStringStack;
    procedure UpdateButtons; // mise à jour des boutons
    procedure StackChanged(Sender: TObject;
      Act: TGVStackNotification); // changement de la pile
  public
    { public declarations }
    function IsEqual: Boolean; // test de l'égalité sur la pile
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
// fermeture de la fiche
begin
  FStackStr.Free; // on libère la pile
  CloseAction := caFree; // on libère la fenêtre
end;

procedure TMainForm.btnPushClick(Sender: TObject);
// actions
var
  St: string;
begin
  case (Sender as TBitBtn).TabOrder of
    0 : begin
          St := 'Item : ' + IntToStr(FStackStr.Count);
          FStackStr.Push(St);
        end;
    1 : St := 'Pop : ' + FStackStr.Pop;
    2 : St := 'Peek : ' + FStackStr.Peek;
    3 : begin
          St := 'Swap';
          FStackStr.Swap;
        end;
    4 : begin
          St := 'Dup';
          FStackStr.Dup;
        end;
    5 : begin
          St := 'Rot';
          FStackStr.Rot;
        end;
    6 : begin
          St := 'Over';
          FStackStr.Over;
        end;
    7 : if IsEqual then
          St := 'Les valeurs sont égales.'
        else
          St := 'Les valeurs sont inégales.';
    8 : begin
          FStackStr.Clear;
          FStackStr.Shrink;
          St := '*** CLEAR ***';
        end;
  end;
  lblCapacity.Caption := 'Capacité de la pile : ' +
    IntToStr(FStackStr.Capacity);
  lblDepth.Caption := 'Profondeur de la pile : ' +
    IntToStr(FStackStr.Count);
  MmoActions.Lines.Append(St);
end;

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  FStackStr := TGVStringStack.Create; // on crée la pile de travail
  FStackStr.OnNotify := @StackChanged;
  Randomize; // initialisation des nombres aléatoires
  UpdateButtons;
end;

procedure TMainForm.UpdateButtons;
// mise à jour des boutons
var
  StackCount : Integer;
  StackEnabled : Boolean;
begin
  StackCount := FStackStr.Count;
  //ProgressBarStack.Position := StackCount;
  StackEnabled := (StackCount <> 0);
  btnPop.Enabled := StackEnabled;
  btnPeek.Enabled := StackEnabled;
  btnDup.Enabled := StackEnabled;
  StackEnabled := (StackCount > 1);
  btnSwap.Enabled := StackEnabled;
  btnEqualP.Enabled := StackEnabled;
  btnOver.Enabled := StackEnabled;
  StackEnabled := (StackCount > 2);
  btnRot.Enabled := StackEnabled;
end;

procedure TMainForm.StackChanged(Sender: TObject; Act: TGVStackNotification);
// changement de la pile
var
  I: Integer;
begin
  for I := 0 to FStackStr.Count - 1 do
    sgStack.Cells[0,I] := FStackStr[I];
  case Act of
    stAdded : begin
      mmoActions.Lines.Add('Un élément a été ajouté à la pile.');
      sgStack.RowCount := FStackStr.Count + 4;
    end;
    stRemoved : begin
      mmoActions.Lines.Add('Un élément a été retiré de la pile.');
      sgStack.Cells[0,FStackStr.Count] := EmptyStr;
    end;
    stChanged : mmoActions.Lines.Add('Le sommet de la pile a été modifié.');
    stCleared: begin
      mmoActions.Lines.Add('La pile est vide.');
      for I := 0 to sgStack.RowCount -1 do
        sgStack.Cells[0,I] := EmptyStr;
    end;
  end;
  UpdateButtons;
end;

function TMainForm.IsEqual: Boolean;
// test de l'égalité sur la pile
var
  St: string;
begin
  with FSTackStr do
  begin
    Over; // duplication de l'avant-dernier
    St := Pop;
    Result := (St = Peek); // on ne retire que celui ajouté
  end;
end;



end.

