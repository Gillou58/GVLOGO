unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    img1: TImage;
    procedure img1DblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.img1DblClick(Sender: TObject);
begin
  img1.Transparent:= true;
  with img1.Picture.Bitmap do
  begin
    Width := Form1.Width;
    Height := Form1.Height;
    Transparent := True;
    TransparentColor := clFuchsia;
    Canvas.Brush.Color := TransparentColor;
    Canvas.FillRect(Rect(0,0,Width,Height));
    Canvas.Pen.Width := 2;
    Canvas.Pen.Color := clLime;
    Canvas.MoveTo(0,0);
    Canvas.LineTo(80,10);
    Canvas.MoveTo(0,0);
    Canvas.LineTo(10,80);

  end;
end;

end.

