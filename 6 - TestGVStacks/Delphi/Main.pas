{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test de l'unité des piles               |
  |                  Unité : TestGVStacks.pas                              |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    22-07-2014 22:52:12                          |
  |                  Version : 2.0.0                                       |
  |                                                                        |
  |========================================================================| }

unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  GVConsts, GVStacks, Vcl.ComCtrls, Vcl.Grids;

type
  TMainForm = class(TForm)
    mmoActions: TMemo;
    grpActions: TGroupBox;
    btnPush: TBitBtn;
    btnPop: TBitBtn;
    btnPeek: TBitBtn;
    btnSwap: TBitBtn;
    btnDup: TBitBtn;
    btnRot: TBitBtn;
    btnOver: TBitBtn;
    btnEqualP: TBitBtn;
    btnClear: TBitBtn;
    lblDepth: TLabel;
    lblCapacity: TLabel;
    statMain: TStatusBar;
    sgStack: TStringGrid;
    procedure btnPushClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    FStackStr: TGVStringStack;
    function IsEqual: Boolean;
    procedure UpdateButtons;
    procedure StackChanged(Sender: TObject; Act: TGVStackNotification);
    // changement de la pile
  public
    { Déclarations publiques }

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnPushClick(Sender: TObject);
var
  St: string;
begin
  case (Sender as TBitBtn).TabOrder of
    0:
      begin
        St := 'Item : ' + IntToStr(FStackStr.Count);
        FStackStr.Push(St);
      end;
    1:
      St := 'Pop : ' + FStackStr.Pop;
    2:
      St := 'Peek : ' + FStackStr.Peek;
    3:
      begin
        St := 'Swap';
        FStackStr.Swap;
      end;
    4:
      begin
        St := 'Dup';
        FStackStr.Dup;
      end;
    5:
      begin
        St := 'Rot';
        FStackStr.Rot;
      end;
    6:
      begin
        St := 'Over';
        FStackStr.Over;
      end;
    7:
      if IsEqual then
        St := 'Les valeurs sont égales.'
      else
        St := 'Les valeurs sont inégales.';
    8:
      begin
        FStackStr.Clear;
        FStackStr.Shrink;
        St := '*** CLEAR ***';
      end;
  end;
  lblCapacity.Caption := 'Capacité de la pile : ' +
    IntToStr(FStackStr.Capacity);
  lblDepth.Caption := 'Profondeur de la pile : ' + IntToStr(FStackStr.Count);
  mmoActions.Lines.Append(St);
end;

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fenêtre
begin
  FStackStr := TGVStringStack.Create; // pile créée
  FStackStr.OnNotify := StackChanged;
  UpdateButtons; // mise à jour des boutons
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fenêtre
begin
  FStackStr.Free; // libération de la pile
end;

function TMainForm.IsEqual: Boolean;
// test de l'égalité au sommet de la pile
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

procedure TMainForm.UpdateButtons;
// mise à jour des boutons
var
  StackCount: Integer;
  StackEnabled: Boolean;
begin
  StackCount := FStackStr.Count;
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

end.
