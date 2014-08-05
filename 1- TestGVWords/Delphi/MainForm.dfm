object FormGVWords: TFormGVWords
  Left = 0
  Top = 0
  ActiveControl = edtOne
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Test de GVWords - V1.0.0 - Projet : GVLOGO'
  ClientHeight = 626
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object grpEdit: TGroupBox
    Left = 0
    Top = 0
    Width = 623
    Height = 113
    Align = alTop
    Caption = 'Saisie '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Font.Quality = fqClearType
    ParentFont = False
    TabOrder = 0
    object lblOne: TLabel
      Left = 24
      Top = 24
      Width = 80
      Height = 16
      Caption = 'Premier mot :'
      FocusControl = edtOne
    end
    object lblTwo: TLabel
      Left = 24
      Top = 64
      Width = 77
      Height = 16
      Caption = 'Second mot :'
      FocusControl = edtTwo
    end
    object lblResult: TLabel
      Left = 344
      Top = 56
      Width = 250
      Height = 33
      Cursor = crHourGlass
      Hint = 'R'#233'sultat de la derni'#232're op'#233'ration'
      AutoSize = False
      Caption = '< r'#233'sultat >'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = []
      Font.Quality = fqClearType
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object edtOne: TEdit
      Left = 113
      Top = 21
      Width = 201
      Height = 24
      Cursor = crHandPoint
      Hint = 'Premier mot '#224' traiter'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Font.Quality = fqClearType
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = #201'l'#233'phant'
    end
    object edtTwo: TEdit
      Left = 113
      Top = 61
      Width = 201
      Height = 24
      Cursor = crHandPoint
      Hint = 'Second mot '#224' traiter'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Font.Quality = fqClearType
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'Libellule'
    end
  end
  object statMain: TStatusBar
    Left = 0
    Top = 607
    Width = 623
    Height = 19
    AutoHint = True
    Panels = <
      item
        Text = 'Test de GVWords'
        Width = 400
      end>
  end
  object grpMisc: TGroupBox
    Left = 0
    Top = 529
    Width = 623
    Height = 78
    Align = alClient
    Caption = 'Divers '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Font.Quality = fqClearType
    ParentFont = False
    TabOrder = 3
    object btnIsValidWord: TButton
      Left = 24
      Top = 32
      Width = 105
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// test de la validit'#233' d'#39'un mot'#13#10'    function IsValid(const St: ' +
        'string): Boolean;'#13#10'   '
      Caption = 'IsValid'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnIsValidWordClick
    end
    object btnIsValidIdentWord: TButton
      Left = 186
      Top = 32
      Width = 105
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// est-ce un identificateur valide ?'#13#10'    function IsValidIdent(' +
        'const St: string): Boolean;'
      Caption = 'IsValidIdent'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnIsValidIdentWordClick
    end
  end
  object grpOne: TGroupBox
    Left = 0
    Top = 113
    Width = 623
    Height = 240
    Align = alTop
    Caption = 'Un mot '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Font.Quality = fqClearType
    ParentFont = False
    TabOrder = 1
    object btnFirst: TButton
      Left = 24
      Top = 32
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// premier caract'#232're d'#39'un mot'#13#10'    function First(const St: stri' +
        'ng): string;'
      Caption = 'First'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnFirstClick
    end
    object btnLast: TButton
      Left = 186
      Top = 32
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// dernier caract'#232're d'#39'un mot'#13#10'    function Last(const St: strin' +
        'g): string;'
      Caption = 'Last'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnLastClick
    end
    object btnButFirst: TButton
      Left = 344
      Top = 32
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// sauf le premier caract'#232're d'#39'un mot'#13#10'    function ButFirst(con' +
        'st St: string): string;'
      Caption = 'ButFirst'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnButFirstClick
    end
    object btnButLast: TButton
      Left = 504
      Top = 32
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// sauf le dernier caract'#232're d'#39'un mot'#13#10'    function ButLast(cons' +
        't St: string): string;'
      Caption = 'ButLast'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btnButLastClick
    end
    object btnLowerCase: TButton
      Left = 24
      Top = 72
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// mot en minuscules'#13#10'    function Lowercase(const St: string): ' +
        'string;'
      Caption = 'LowerCase'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = btnLowerCaseClick
    end
    object btnUppercase: TButton
      Left = 186
      Top = 72
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// mot en majuscules'#13#10'    function Uppercase(const St: string): ' +
        'string;'
      Caption = 'Uppercase'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnUppercaseClick
    end
    object btnEmptyWordP: TButton
      Left = 344
      Top = 72
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// le mot est-il vide ?'#13#10'    function EmptyWordP(const St: strin' +
        'g): Boolean;'
      Caption = 'EmptyWordP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = btnEmptyWordPClick
    end
    object btnWithoutEsc: TButton
      Left = 504
      Top = 72
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// supprime le caract'#232're d'#39#233'chappement d'#39'un mot'#13#10'    function Wi' +
        'thoutEsc(const St: string): string;'
      Caption = 'WithoutEsc'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = btnWithoutEscClick
    end
    object btnWithoutQuote: TButton
      Left = 24
      Top = 112
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// supprime si n'#233'cessaire le " initial d'#39'un mot'#13#10'    function Wi' +
        'thoutQuote(const St: string): string;'
      Caption = 'WithoutQuote'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = btnWithoutQuoteClick
    end
    object btnWithoutColon: TButton
      Left = 186
      Top = 112
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// supprime si n'#233'cessaire le : initial d'#39'un mot'#13#10'    function Wi' +
        'thoutColon(const St: string): string;'
      Caption = 'WithoutColon'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = btnWithoutColonClick
    end
    object btnWithEsc: TButton
      Left = 344
      Top = 112
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// normalise un mot en tenant compte du caract'#232're d'#39#233'chappement'#13 +
        #10'    function WithEsc(const St: string): string;'
      Caption = 'WithEsc'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = btnWithEscClick
    end
    object btnCount: TButton
      Left = 504
      Top = 112
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// longueur du mot'#13#10'    function Count(const St: string): Intege' +
        'r;'
      Caption = 'Count'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = btnCountClick
    end
    object btnReverse: TButton
      Left = 24
      Top = 152
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = '// mot invers'#233#13#10'    function Reverse(const St: string): string;'
      Caption = 'Reverse'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = btnReverseClick
    end
    object btnAtRandom: TButton
      Left = 186
      Top = 152
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// lettre au hasard'#13#10'    function AtRandom(const St: string): st' +
        'ring;'
      Caption = 'AtRandom'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = btnAtRandomClick
    end
    object btnShuffle: TButton
      Left = 344
      Top = 152
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// mot m'#233'lang'#233#13#10'    function Shuffle(const St: string): string; ' +
        'overload;'
      Caption = 'Shuffle'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = btnShuffleClick
    end
    object btnSort: TButton
      Left = 504
      Top = 152
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// tri des lettres du mot'#13#10'    function Sort(const St: string): ' +
        'string;'
      Caption = 'Sort'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      OnClick = btnSortClick
    end
    object btnNumberP: TButton
      Left = 24
      Top = 192
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// le mot est-il un nombre ?'#13#10'    function NumberP(const St: str' +
        'ing): Boolean;'
      Caption = 'NumberP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnClick = btnNumberPClick
    end
    object seItem: TSpinEdit
      Left = 504
      Top = 192
      Width = 90
      Height = 26
      Hint = 'El'#233'ment '#224' traiter pour Item'
      MaxValue = 100
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      Value = 1
      OnChange = btnItemClick
    end
    object btnItem: TButton
      Left = 344
      Top = 192
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// '#233'l'#233'ment N d'#39'un mot'#13#10'    function Item(const St: string; N: In' +
        'teger): string;'
      Caption = 'Item'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 18
      OnClick = btnItemClick
    end
    object btnRotate: TButton
      Left = 186
      Top = 192
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Caption = 'Rotate'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnClick = btnRotateClick
    end
  end
  object grpTwo: TGroupBox
    Left = 0
    Top = 353
    Width = 623
    Height = 176
    Align = alTop
    Caption = 'Deux mots '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Font.Quality = fqClearType
    ParentFont = False
    TabOrder = 2
    object btnEqualP: TButton
      Left = 24
      Top = 40
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// les deux mots sont-ils '#233'gaux ?'#13#10'    function EqualP(const StF' +
        'irst, StTwo: string): Boolean;'
      Caption = 'EqualP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnEqualPClick
    end
    object btnGreaterP: TButton
      Left = 186
      Top = 40
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// le premier mot est-il '#224' placer apr'#232's le second ?'#13#10'    functio' +
        'n GreaterP(const StFirst, StTwo: string): Boolean;'
      Caption = 'GreaterP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnGreaterPClick
    end
    object btnLowerP: TButton
      Left = 344
      Top = 40
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// le premier mot est-il '#224' placer avant le second ?'#13#10'    functio' +
        'n LowerP(const StFirst, StTwo: string): Boolean;'
      Caption = 'LowerP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnLowerPClick
    end
    object btnMemberP: TButton
      Left = 504
      Top = 40
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// le mot est-il compris dans un autre ?'#13#10'    function MemberP(c' +
        'onst St, SubSt: string): Boolean;'
      Caption = 'MemberP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btnMemberPClick
    end
    object btnGreatest: TButton
      Left = 24
      Top = 80
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie le mot qui vient apr'#232's'#13#10'    function Greatest(const S' +
        'tFirst, StTwo: string): string;'
      Caption = 'Greatest'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = btnGreatestClick
    end
    object btnLowest: TButton
      Left = 186
      Top = 80
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie le mot qui vient avant'#13#10'    function Lowest(const StF' +
        'irst, StTwo: string): string;'
      Caption = 'Lowest'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnLowestClick
    end
    object btnPutFirst: TButton
      Left = 344
      Top = 80
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// met en premier SubSt'#13#10'    function PutFirst(const StOne, StTw' +
        'o: string): string;'
      Caption = 'PutFirst'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = btnPutFirstClick
    end
    object btnPutLast: TButton
      Left = 504
      Top = 80
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// met en dernier SubSt'#13#10'    function PutLast(const StOne, StTwo' +
        ': string): string;'
      Caption = 'PutLast'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = btnPutLastClick
    end
    object btnReplace: TButton
      Left = 24
      Top = 120
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// remplacement de l'#39#233'l'#233'ment N d'#39'un mot'#13#10'    function Replace(co' +
        'nst St, SubSt: string; N: Integer): string;'
      Caption = 'Replace'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = btnReplaceClick
    end
    object seReplace: TSpinEdit
      Left = 186
      Top = 120
      Width = 90
      Height = 26
      Cursor = crIBeam
      Hint = 'El'#233'ment '#224' traiter pour Replace'
      MaxValue = 100
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      Value = 1
      OnChange = btnReplaceClick
    end
    object btnInsert: TButton
      Left = 344
      Top = 120
      Width = 90
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// insertion en position N'#13#10'    function Insert(const St, SubSt:' +
        ' string; N: Integer): string;'
      Caption = 'Insert'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = btnInsertClick
    end
    object seInsert: TSpinEdit
      Left = 504
      Top = 120
      Width = 90
      Height = 26
      Cursor = crIBeam
      Hint = 'El'#233'ment '#224' traiter pour Insert'
      MaxValue = 100
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      Value = 1
      OnChange = btnInsertClick
    end
  end
end
