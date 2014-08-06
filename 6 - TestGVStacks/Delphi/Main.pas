{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test de l'unit� des piles               |
  |                  Unit� : TestGVStacks.pas                              |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR                              |
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
    { D�clarations priv�es }
    FStackStr: TGVStringStack;
    function IsEqual: Boolean;
    procedure UpdateButtons;
    procedure StackChanged(Sender: TObject; Act: TGVStackNotification);
    // changement de la pile
  public
    { D�clarations publiques }

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
        St := 'Les valeurs sont �gales.'
      else
        St := 'Les valeurs sont in�gales.';
    8:
      begin
        FStackStr.Clear;
        FStackStr.Shrink;
        St := '*** CLEAR ***';
      end;
  end;
  lblCapacity.Caption := 'Capacit� de la pile : ' +
    IntToStr(FStackStr.Capacity);
  lblDepth.Caption := 'Profondeur de la pile : ' + IntToStr(FStackStr.Count);
  mmoActions.Lines.Append(St);
end;

procedure TMainForm.FormCreate(Sender: TObject);
// cr�ation de la fen�tre
begin
  FStackStr := TGVStringStack.Create; // pile cr��e
  FStackStr.OnNotify := StackChanged;
  UpdateButtons; // mise � jour des boutons
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fen�tre
begin
  FStackStr.Free; // lib�ration de la pile
end;

function TMainForm.IsEqual: Boolean;
// test de l'�galit� au sommet de la pile
// test de l'�galit� sur la pile
var
  St: string;
begin
  with FSTackStr do
  begin
    Over; // duplication de l'avant-dernier
    St := Pop;
    Result := (St = Peek); // on ne retire que celui ajout�
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
      mmoActions.Lines.Add('Un �l�ment a �t� ajout� � la pile.');
      sgStack.RowCount := FStackStr.Count + 4;
    end;
    stRemoved : begin
      mmoActions.Lines.Add('Un �l�ment a �t� retir� de la pile.');
      sgStack.Cells[0,FStackStr.Count] := EmptyStr;
    end;
    stChanged : mmoActions.Lines.Add('Le sommet de la pile a �t� modifi�.');
    stCleared: begin
      mmoActions.Lines.Add('La pile est vide.');
      for I := 0 to sgStack.RowCount -1 do
        sgStack.Cells[0,I] := EmptyStr;
    end;
  end;
  UpdateButtons;
end;

procedure TMainForm.UpdateButtons;
// mise � jour des boutons
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
