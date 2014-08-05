{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test du traitement des listes           |
  |                  Unité : TestGVLists.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    19-06-2014 16:28:55                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

unit MainForm;
{$DEFINE Delphi}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  GVConsts, GVWords, GVLists, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Samples.Spin,
  Vcl.Buttons;

type
  TMainFormGVLists = class(TForm)
    mmoMain: TMemo;
    statMain: TStatusBar;
    grpEdit: TGroupBox;
    lblList: TLabel;
    edtList: TEdit;
    lblStr: TLabel;
    edtStr: TEdit;
    seNum: TSpinEdit;
    lblNum: TLabel;
    btnClear: TBitBtn;
    grpListUtils: TGroupBox;
    grpGVList: TGroupBox;
    btnListToStr: TButton;
    btnStrToList: TButton;
    btnListToWord: TButton;
    btnWordToList: TButton;
    btnEmptyList: TButton;
    btnIsSimpleList: TButton;
    btnIsValid: TButton;
    btnIsValidValue: TButton;
    btnTestValue: TButton;
    btnFirst: TButton;
    btnButFirst: TButton;
    btnLast: TButton;
    btnButLast: TButton;
    btnPutFirst: TButton;
    btnPutLast: TButton;
    btnSentenceLeft: TButton;
    btnSentenceRight: TButton;
    btnUpperCase: TButton;
    btnLowerCase: TButton;
    btnTwoAdd: TButton;
    btnTwoDelete: TButton;
    btnReverseItems: TButton;
    btnSortItems: TButton;
    btnShuffleItems: TButton;
    btnToStr: TButton;
    btnToWBStr: TButton;
    btnDeleteItem: TButton;
    btnInsertAItem: TButton;
    btnReplaceItem: TButton;
    btnIsItem: TButton;
    btnIsEmpty: TButton;
    btnIsEmptyList: TButton;
    btnN: TButton;
    btnCount: TButton;
    btnLastItem: TButton;
    btnLastErrorPos: TButton;
    btnRotate: TButton;
    procedure btnClearClick(Sender: TObject);
    procedure btnListToStrClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStrToListClick(Sender: TObject);
    procedure btnListToWordClick(Sender: TObject);
    procedure btnWordToListClick(Sender: TObject);
    procedure btnEmptyListClick(Sender: TObject);
    procedure btnIsSimpleListClick(Sender: TObject);
    procedure btnIsValidClick(Sender: TObject);
    procedure btnIsValidValueClick(Sender: TObject);
    procedure btnTestValueClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnButFirstClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnButLastClick(Sender: TObject);
    procedure btnPutFirstClick(Sender: TObject);
    procedure btnPutLastClick(Sender: TObject);
    procedure btnSentenceLeftClick(Sender: TObject);
    procedure btnSentenceRightClick(Sender: TObject);
    procedure btnUpperCaseClick(Sender: TObject);
    procedure btnLowerCaseClick(Sender: TObject);
    procedure btnTwoAddClick(Sender: TObject);
    procedure btnTwoDeleteClick(Sender: TObject);
    procedure btnReverseItemsClick(Sender: TObject);
    procedure btnSortItemsClick(Sender: TObject);
    procedure btnShuffleItemsClick(Sender: TObject);
    procedure btnToStrClick(Sender: TObject);
    procedure btnToWBStrClick(Sender: TObject);
    procedure btnDeleteItemClick(Sender: TObject);
    procedure btnInsertAItemClick(Sender: TObject);
    procedure btnReplaceItemClick(Sender: TObject);
    procedure btnIsItemClick(Sender: TObject);
    procedure btnIsEmptyClick(Sender: TObject);
    procedure btnIsEmptyListClick(Sender: TObject);
    procedure btnNClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnLastItemClick(Sender: TObject);
    procedure btnLastErrorPosClick(Sender: TObject);
    procedure btnRotateClick(Sender: TObject);
  private
    { Déclarations privées }
    fList: TGVList; // liste de travail
    fWord: TGVWord; // mot de travail
    fUtils: TGVListUtils; // utilitaires de travail
  public
    { Déclarations publiques }
  end;

var
  MainFormGVLists: TMainFormGVLists;

implementation

{$R *.dfm}
uses StrUtils;

procedure TMainFormGVLists.btnButFirstClick(Sender: TObject);
// test de BUTFIRST
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.ButFirst);
end;

procedure TMainFormGVLists.btnButLastClick(Sender: TObject);
// test de BUTLAST
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.ButLast);
end;

procedure TMainFormGVLists.btnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  mmoMain.Lines.Clear;
end;

procedure TMainFormGVLists.btnCountClick(Sender: TObject);
// test de COUNT
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(IntToStr(fList.Count));
end;

procedure TMainFormGVLists.btnDeleteItemClick(Sender: TObject);
// test de DELETEITEM
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.DeleteItem(seNum.Value));
end;

procedure TMainFormGVLists.btnEmptyListClick(Sender: TObject);
// test de EMPTYLIST
begin
  mmoMain.Lines.Add(fUtils.EmptyList);
end;

procedure TMainFormGVLists.btnFirstClick(Sender: TObject);
// test de FIRST
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.First);
end;

procedure TMainFormGVLists.btnInsertAItemClick(Sender: TObject);
// test de INSERTAITEM
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.InsertAItem(seNum.Value, edtStr.Text));
end;

procedure TMainFormGVLists.btnIsEmptyClick(Sender: TObject);
// test de ISEMPTY
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(IfThen(fList.IsEmpty, P_True, P_False));
end;

procedure TMainFormGVLists.btnIsEmptyListClick(Sender: TObject);
// test de ISEMPTYLIST
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(IfThen(fList.IsEmptyList, P_True, P_False));
end;

procedure TMainFormGVLists.btnIsItemClick(Sender: TObject);
// test de ISITEM
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(IfThen(fList.IsItem(edtStr.Text), P_True, P_False));
end;

procedure TMainFormGVLists.btnIsSimpleListClick(Sender: TObject);
// test de ISSIMPLELIST
begin
  mmoMain.Lines.Add(IfThen(fUtils.IsSimpleList(edtList.Text), P_True, P_False));
end;

procedure TMainFormGVLists.btnIsValidClick(Sender: TObject);
// test de ISVALID
begin
  mmoMain.Lines.Add(IfThen(fUtils.IsValid(edtList.Text), P_True, P_False));
end;

procedure TMainFormGVLists.btnIsValidValueClick(Sender: TObject);
// test de ISVALIDVALUE
begin
  mmoMain.Lines.Add(IfThen(fUtils.IsValidValue(edtList.Text), P_True, P_False));
end;

procedure TMainFormGVLists.btnLastClick(Sender: TObject);
// test de LAST
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.Last);
end;

procedure TMainFormGVLists.btnLastErrorPosClick(Sender: TObject);
// test de LASTERRORPOS
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(IntToStr(fList.LastErrorPos));
end;

procedure TMainFormGVLists.btnLastItemClick(Sender: TObject);
// test de LASTITEM
begin
  mmoMain.Lines.Add(IntToStr(fList.LastItem));
end;

procedure TMainFormGVLists.btnListToStrClick(Sender: TObject);
// test de LISTTOSTR
begin
  mmoMain.Lines.Add(fUtils.ListToStr(edtList.Text));
end;

procedure TMainFormGVLists.btnListToWordClick(Sender: TObject);
// test de LISTTOWORD
begin
  mmoMain.Lines.Add(fUtils.ListToWord(edtList.Text));
end;

procedure TMainFormGVLists.btnLowerCaseClick(Sender: TObject);
// test de LOWERCASE
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.LowerCase);
end;

procedure TMainFormGVLists.btnNClick(Sender: TObject);
// test de [N]
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList[seNum.Value]);
end;

procedure TMainFormGVLists.btnPutFirstClick(Sender: TObject);
// test de PUTFIRST
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.PutFirst(edtStr.Text));
end;

procedure TMainFormGVLists.btnPutLastClick(Sender: TObject);
// test de PUTLAST
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.PutLast(edtStr.Text));
end;

procedure TMainFormGVLists.btnReplaceItemClick(Sender: TObject);
// test de REPLACEITEM
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.ReplaceItem(seNum.Value, edtStr.Text));
end;

procedure TMainFormGVLists.btnReverseItemsClick(Sender: TObject);
// test de REVERSEITEMS
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.ReverseItems);
end;

procedure TMainFormGVLists.btnRotateClick(Sender: TObject);
// test de ROTATE
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.Rotate);
end;

procedure TMainFormGVLists.btnSentenceLeftClick(Sender: TObject);
// test de SENTENCELEFT
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.SentenceLeft(edtStr.Text));
end;

procedure TMainFormGVLists.btnSentenceRightClick(Sender: TObject);
// test de SENTENCERIGHT
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.SentenceRight(edtStr.Text));
end;

procedure TMainFormGVLists.btnShuffleItemsClick(Sender: TObject);
// test de SHUFFLEITEMS
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.ShuffleItems);
end;

procedure TMainFormGVLists.btnSortItemsClick(Sender: TObject);
// test de SORTITEMS
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.SortItems);
end;

procedure TMainFormGVLists.btnStrToListClick(Sender: TObject);
// test de STRTOLIST
begin
  mmoMain.Lines.Add(fUtils.StrToList(edtStr.Text));
end;

procedure TMainFormGVLists.btnTestValueClick(Sender: TObject);
// test de TESTVALUE
begin
  fUtils.TestValue(edtList.Text);
  mmoMain.Lines.Add(ME_None); // pas d'erreur
end;

procedure TMainFormGVLists.btnToStrClick(Sender: TObject);
// test de TOSTR
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.ToStr);
end;

procedure TMainFormGVLists.btnToWBStrClick(Sender: TObject);
// test de TOWBSTR
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.ToWBStr);
end;

procedure TMainFormGVLists.btnTwoAddClick(Sender: TObject);
// test de TWOADD
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.TwoAdd(edtStr.Text, edtStr.Text));
end;

procedure TMainFormGVLists.btnTwoDeleteClick(Sender: TObject);
// test de TWODELETE
begin
 fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.TwoDelete(seNum.Value));
end;

procedure TMainFormGVLists.btnUpperCaseClick(Sender: TObject);
// test de UPPERCASE
begin
  fList.Text := edtList.Text;
  mmoMain.Lines.Add(fList.UpperCase);
end;

procedure TMainFormGVLists.btnWordToListClick(Sender: TObject);
// test de WORDTOLIST
begin
  mmoMain.Lines.Add(fUtils.WordToList(edtStr.Text));
end;

procedure TMainFormGVLists.FormCreate(Sender: TObject);
// création de la fiche
begin
  fList := TGVList.Create; // la liste
  fWord := TGVWord.Create; // un mot
  fUtils := TGVListUtils.Create; // les utilitaires
end;

procedure TMainFormGVLists.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  fList.Free; // libération des objets
  fWord.Free;
  fUtils.Free;
end;

end.
