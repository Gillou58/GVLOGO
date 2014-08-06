unit Main;
{  Test des piles (c) Gilles Vasseur 2013 version : 0.1
   20/12/2013

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>. }


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Generics.Collections, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Grids;

type
  ExtStack<T> = class(TStack<T>)
  private
  public
    procedure Swap;
    procedure Dup;
    procedure Over;
    procedure Rot;
  end;

  TFormMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    MemoStackActions: TMemo;
    BitBtnClose: TBitBtn;
    BitBtnPush: TBitBtn;
    BitBtnPop: TBitBtn;
    BitBtnPeek: TBitBtn;
    BitBtnSwap: TBitBtn;
    BitBtnDup: TBitBtn;
    BitBtnRot: TBitBtn;
    BitBtnEqual: TBitBtn;
    ProgressBarStack: TProgressBar;
    BitBtnClear: TBitBtn;
    StaticTextCount: TStaticText;
    StaticTextCap: TStaticText;
    StaticTextCountC: TStaticText;
    StaticTextCapC: TStaticText;
    BitBtnOver: TBitBtn;
    Panel: TPanel;
    StringGridStack: TStringGrid;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtnPopClick(Sender: TObject);
  private
    { Déclarations privées }
    FStr : string;
    FStrStack : ExtStack<string>;
    procedure ControlsUpdate;
    procedure StackChanged(Sender: TObject; const Item: String; Action: TCollectionNotification);
  public
    { Déclarations publiques }
    function IsEqual : Boolean;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{ StrStack }

procedure ExtStack<T>.Dup;
// duplicate the top of the stack
begin
  Push(Peek);
end;

procedure ExtStack<T>.Over;
// duplicate the second item of the stack onto top
var
  Item1,Item2 : T;
begin
  Item1 := Pop;
  Item2 := Peek;
  Push(Item1);
  Push(Item2);
end;

procedure ExtStack<T>.Rot;
// rotate the first three items of the stack
var
  Item1,Item2,Item3 : T;
begin
  Item1 := Pop;
  Item2 := Pop;
  Item3 := Pop;
  Push(Item2);
  Push(Item1);
  Push(Item3);
end;

procedure ExtStack<T>.Swap;
// swap the first two items on the stack
var
  Item1,Item2 : T;
begin
  Item1 := Pop;
  Item2 := Pop;
  Push(Item1);
  Push(Item2);
end;

procedure TFormMain.BitBtnCloseClick(Sender: TObject);
// end of the application
begin
  FStrStack.Clear;
  FStrStack.Free;
  Application.Terminate;
end;

procedure TFormMain.BitBtnPopClick(Sender: TObject);
begin
  case (Sender as TBitBtn).TabOrder of
    0 : begin
          FStr := 'Item : ' + IntToStr(FStrStack.Count);
          FStrStack.Push(FStr);
        end;
    1 : FStr := 'Pop : ' + FStrStack.Pop;
    2 : FStr := 'Peek : ' + FStrStack.Peek;
    3 : begin
          FStr := 'Swap';
          FStrStack.Swap;
        end;
    4 : begin
          FStr := 'Dup';
          FStrStack.Dup;
        end;
    5 : begin
          FStr := 'Rot';
          FStrStack.Rot;
        end;
    6 : begin
          FStr := 'Over';
          FStrStack.Over;
        end;
    7 : if IsEqual then
          FStr := 'Les valeurs sont égales.'
        else
          FStr := 'Les valeurs sont inégales.';
    8 : begin
          FStrStack.Clear;
          FStrStack.TrimExcess;
          StringGridStack.RowCount := FStrStack.Capacity + 1;
          StaticTextCap.Caption := IntToStr(FStrStack.Capacity);
          FStr := '*** CLEAR ***';
        end;
  end;
  MemoStackActions.Lines.Append(FStr);
end;

procedure TFormMain.ControlsUpdate;
// update controls
var
  StackCount : Integer;
  StackEnabled : Boolean;
begin
  StackCount := FStrStack.Count;
  ProgressBarStack.Position := StackCount;
  StaticTextCount.Caption := IntToStr(StackCount);
  StaticTextCap.Caption := IntToStr(FStrStack.Capacity);
  StackEnabled := (StackCount <> 0);
  BitBtnPop.Enabled := StackEnabled;
  BitBtnPeek.Enabled := StackEnabled;
  BitBtnDup.Enabled := StackEnabled;
  StackEnabled := (StackCount > 1);
  BitBtnSwap.Enabled := StackEnabled;
  BitBtnEqual.Enabled := StackEnabled;
  BitBtnOver.Enabled := StackEnabled;
  StackEnabled := (StackCount > 2);
  BitBtnRot.Enabled := StackEnabled;
end;

procedure TFormMain.FormCreate(Sender: TObject);
// creation of the form
begin
  FStrStack := ExtStack<string>.Create(); // creation of the stack
  FStrStack.OnNotify := StackChanged;
  StringGridStack.Cells[0,0] := 'Pile';
  ControlsUpdate;
end;

function TFormMain.IsEqual: Boolean;
// two last values on stack equal ?
begin
  with FStrStack do
  begin
    Over;
    Result := (Pop = Peek);
  end;
end;

procedure TFormMain.StackChanged(Sender: TObject; const Item: String;
  Action: TCollectionNotification);
// stack changed
begin
  case Action of
    cnAdded : begin
                StringGridStack.Cells[0,FStrStack.Count] := Item;
                StringGridStack.RowCount := FStrStack.Capacity + 1;
              end;
    cnRemoved, cnExtracted : StringGridStack.Cells[0,FStrStack.Count+1] := EmptyStr;
  end;
  ControlsUpdate;
end;

end.
