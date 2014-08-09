unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
uses
  lazutf8;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Label1.Caption:= IntToStr(Length(Edit1.text));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
   Label2.Caption:= IntToStr(UTF8Length(Edit1.text));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  I: Integer;
  St: string;
begin
  St := Edit1.Text;
  Label3.Caption := '';
  for I := 1 to Length(St) do
     Label3.Caption:= Label3.Caption + St[I] + ' ';
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  I: Integer;
  St: string;
begin
  St := Edit1.Text;
  Label4.Caption := '';
  for I := 1 to UTF8Length(St) do
     Label4.Caption:= Label4.Caption + UTF8Copy(St,I,1) + ' ';
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  C: Char;
begin
  Label5.Caption := '';
  for C in Edit1.Text do
    Label5.Caption:= Label5.Caption + C + ' ';
end;

end.

