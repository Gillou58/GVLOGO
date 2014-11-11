object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 402
  ClientWidth = 672
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 328
    Top = 96
    Width = 3
    Height = 13
  end
  object Memo1: TMemo
    Left = 8
    Top = 88
    Width = 281
    Height = 306
    Lines.Strings = (
      '< '#233'l'#233'ments de l'#39'expression>')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 576
    Top = 86
    Width = 75
    Height = 25
    Caption = 'Analyse'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 643
    Height = 21
    TabOrder = 2
    Text = '128.96'
  end
end
