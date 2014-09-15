unit maintokens;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GVtokens;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    Tok: TGVTokens;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
// création de la fiche
begin
  Tok := TGVTokens.Create;
end;

procedure TForm1.Button1Click(Sender: TObject);
// énumération des éléments
var
  St: string;
begin
  Memo1.Clear;
  Tok.Source := Edit1.Text;
  Tok.Tokenize;
  for St in Tok do
    Memo1.Lines.Add(St);
end;

procedure TForm1.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  Tok.Free;
end;

end.

