object MainFormGVLists: TMainFormGVLists
  Left = 0
  Top = 0
  ActiveControl = edtList
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Test de GVLists - V1.0.0 - GVLOGO'
  ClientHeight = 665
  ClientWidth = 845
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
  object mmoMain: TMemo
    Left = 8
    Top = 16
    Width = 401
    Height = 593
    Cursor = crNo
    Hint = 'Zone d'#39'affichage.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 0
  end
  object statMain: TStatusBar
    Left = 0
    Top = 646
    Width = 845
    Height = 19
    AutoHint = True
    Panels = <>
    ParentShowHint = False
    ShowHint = False
    SimplePanel = True
  end
  object grpEdit: TGroupBox
    Left = 415
    Top = 16
    Width = 422
    Height = 137
    Caption = 'Saisie '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object lblList: TLabel
      Left = 11
      Top = 32
      Width = 77
      Height = 13
      Caption = 'Liste de travail :'
      FocusControl = edtList
    end
    object lblStr: TLabel
      Left = 11
      Top = 59
      Width = 88
      Height = 13
      Caption = 'Cha'#238'ne de travail :'
    end
    object lblNum: TLabel
      Left = 11
      Top = 86
      Width = 44
      Height = 13
      Caption = 'Num'#233'ro :'
    end
    object edtList: TEdit
      Left = 104
      Top = 29
      Width = 304
      Height = 21
      Cursor = crHandPoint
      Hint = 'Saisissez une liste.'
      AutoSize = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = '[ '#233'l'#233'ment1 ['#233'l'#233'ment2a '#233'l'#233'ment 2b] '#233'l'#233'ment3 '#233'l'#233'ment4]'
    end
    object edtStr: TEdit
      Left = 105
      Top = 56
      Width = 303
      Height = 21
      Cursor = crHandPoint
      Hint = 'Saisissez la cha'#238'ne '#224' traiter.'
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '[nouveau]'
    end
    object seNum: TSpinEdit
      Left = 104
      Top = 83
      Width = 121
      Height = 22
      Cursor = crHandPoint
      Hint = 'Saisissez le num'#233'ro de l'#39#233'l'#233'ment '#224' traiter (le premier vaut 0).'
      MaxValue = 50
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Value = 1
    end
  end
  object btnClear: TBitBtn
    Left = 334
    Top = 615
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Hint = 'Nettoyage de l'#39#233'diteur.'
    Caption = 'Nettoyer'
    Kind = bkCancel
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = btnClearClick
  end
  object grpListUtils: TGroupBox
    Left = 415
    Top = 159
    Width = 422
    Height = 130
    Caption = 'TGVListUtils '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object btnListToStr: TButton
      Left = 16
      Top = 32
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// conversion d'#39'une liste en cha'#238'ne'#13#10'    function ListToStr(cons' +
        't St: string): string;'
      Caption = 'ListToStr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnListToStrClick
    end
    object btnStrToList: TButton
      Left = 120
      Top = 32
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// conversion d'#39'une cha'#238'ne en liste'#13#10'    function StrToList(cons' +
        't St: string): string;'
      Caption = 'StrToList'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnStrToListClick
    end
    object btnListToWord: TButton
      Left = 224
      Top = 32
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// conversion d'#39'une liste en mot'#13#10'    function ListToWord(const ' +
        'St: string): string;'
      Caption = 'ListToWord'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnListToWordClick
    end
    object btnWordToList: TButton
      Left = 328
      Top = 32
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// conversion d'#39'un mot en liste'#13#10'    function WordToList(const S' +
        't: string): string;'
      Caption = 'WordToList'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btnWordToListClick
    end
    object btnEmptyList: TButton
      Left = 16
      Top = 63
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// retourne la liste vide'#13#10'    function EmptyList: string;'
      Caption = 'EmptyList'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = btnEmptyListClick
    end
    object btnIsSimpleList: TButton
      Left = 120
      Top = 63
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// liste simple ?'#13#10'    function IsSimpleList(const St: string): ' +
        'Boolean;'
      Caption = 'IsSimpleList'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnIsSimpleListClick
    end
    object btnIsValid: TButton
      Left = 224
      Top = 63
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// v'#233'rifie la validit'#233' d'#39'une liste'#13#10'    function IsValid(const S' +
        't: string): Boolean;'
      Caption = 'IsValid'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = btnIsValidClick
    end
    object btnIsValidValue: TButton
      Left = 328
      Top = 63
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// teste la validit'#233' d'#39'une valeur (mot ou liste) - sans exceptio' +
        'n'#13#10'    function IsValidValue(const St: string): Boolean;'
      Caption = 'IsValidValue'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = btnIsValidValueClick
    end
    object btnTestValue: TButton
      Left = 16
      Top = 94
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// teste la validit'#233' d'#39'une valeur (mot ou liste)'#13#10'    procedure ' +
        'TestValue(const St: string);'
      Caption = 'TestValue'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = btnTestValueClick
    end
  end
  object grpGVList: TGroupBox
    Left = 415
    Top = 295
    Width = 422
    Height = 314
    Caption = 'TGVList '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    object btnFirst: TButton
      Left = 16
      Top = 24
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie le premier '#233'l'#233'ment de la liste'#13#10'    function First: s' +
        'tring;'
      Caption = 'First'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnFirstClick
    end
    object btnButFirst: TButton
      Left = 120
      Top = 24
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// sauf le premier de la liste'#13#10'    function ButFirst: string;'
      Caption = 'ButFirst'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnButFirstClick
    end
    object btnLast: TButton
      Left = 224
      Top = 24
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie le dernier '#233'l'#233'ment de la liste'#13#10'    function Last: st' +
        'ring;'
      Caption = 'Last'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnLastClick
    end
    object btnButLast: TButton
      Left = 328
      Top = 24
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// sauf le dernier de la liste'#13#10'    function ButLast: string;'
      Caption = 'ButLast'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btnButLastClick
    end
    object btnPutFirst: TButton
      Left = 16
      Top = 64
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// met en premier'#13#10'    function PutFirst(const St: string): stri' +
        'ng;'
      Caption = 'PutFirst'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = btnPutFirstClick
    end
    object btnPutLast: TButton
      Left = 120
      Top = 64
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// met en dernier'#13#10'    function PutLast(const St: string): strin' +
        'g;'
      Caption = 'PutLast'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnPutLastClick
    end
    object btnSentenceLeft: TButton
      Left = 224
      Top = 64
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// phrase '#224' gauche'#13#10'    function SentenceLeft(const St: string):' +
        ' string;'
      Caption = 'SentenceLeft'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = btnSentenceLeftClick
    end
    object btnSentenceRight: TButton
      Left = 328
      Top = 64
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// phrase '#224' droite'#13#10'    function SentenceRight(const St: string)' +
        ': string;'
      Caption = 'SentenceRight'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = btnSentenceRightClick
    end
    object btnUpperCase: TButton
      Left = 16
      Top = 104
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// liste en majuscules'#13#10'    function UpperCase: string;'
      Caption = 'UpperCase'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = btnUpperCaseClick
    end
    object btnLowerCase: TButton
      Left = 120
      Top = 104
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// liste en minuscules'#13#10'    function LowerCase: string;'
      Caption = 'LowerCase'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = btnLowerCaseClick
    end
    object btnTwoAdd: TButton
      Left = 224
      Top = 104
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// ajout d'#39'une paire'#13#10'    function TwoAdd(const St1, St2: string' +
        '): string;'
      Caption = 'TwoAdd'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = btnTwoAddClick
    end
    object btnTwoDelete: TButton
      Left = 328
      Top = 104
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// suppression d'#39'une paire'#13#10'    function TwoDelete(N: Integer): ' +
        'string;'
      Caption = 'TwoDelete'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = btnTwoDeleteClick
    end
    object btnReverseItems: TButton
      Left = 16
      Top = 144
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// inversion des '#233'l'#233'ments'#13#10'    function ReverseItems: string;'
      Caption = 'ReverseItems'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = btnReverseItemsClick
    end
    object btnSortItems: TButton
      Left = 120
      Top = 144
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// tri des '#233'l'#233'ments'#13#10'    function SortItems: string;'
      Caption = 'SortItems'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = btnSortItemsClick
    end
    object btnShuffleItems: TButton
      Left = 224
      Top = 144
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// m'#233'lange des '#233'l'#233'ments'#13#10'    function ShuffleItems: string;'
      Caption = 'ShuffleItems'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = btnShuffleItemsClick
    end
    object btnToStr: TButton
      Left = 328
      Top = 144
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie la liste sous forme de cha'#238'ne'#13#10'    function ToStr: st' +
        'ring;'
      Caption = 'ToStr'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      OnClick = btnToStrClick
    end
    object btnToWBStr: TButton
      Left = 16
      Top = 184
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie la liste sous forme de cha'#238'ne sans crochets'#13#10'    func' +
        'tion ToWBStr: string;'
      Caption = 'ToWBStr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnClick = btnToWBStrClick
    end
    object btnDeleteItem: TButton
      Left = 120
      Top = 184
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// supprime l'#39#233'l'#233'ment N'#13#10'    function DeleteItem(N: Integer): st' +
        'ring;'
      Caption = 'DeleteItem'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnClick = btnDeleteItemClick
    end
    object btnInsertAItem: TButton
      Left = 224
      Top = 184
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// insertion d'#39'un '#233'l'#233'ment en position N'#13#10'    function InsertAIte' +
        'm(N: Integer; const St: string): string;'
      Caption = 'InsertAItem'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 18
      OnClick = btnInsertAItemClick
    end
    object btnReplaceItem: TButton
      Left = 328
      Top = 184
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// remplacement de l'#39#233'l'#233'ment N'#13#10'    function ReplaceItem(N: Inte' +
        'ger; const St: string): string;'
      Caption = 'ReplaceItem'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = btnReplaceItemClick
    end
    object btnIsItem: TButton
      Left = 16
      Top = 224
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// membre pr'#233'sent ?'#13#10'    function IsItem(const St: string): Bool' +
        'ean;'
      Caption = 'IsItem'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 20
      OnClick = btnIsItemClick
    end
    object btnIsEmpty: TButton
      Left = 120
      Top = 224
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// la liste est-elle vide ?'#13#10'    function IsEmpty: Boolean;'
      Caption = 'IsEmpty'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 21
      OnClick = btnIsEmptyClick
    end
    object btnIsEmptyList: TButton
      Left = 224
      Top = 224
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// la liste est-elle la liste vide ?'#13#10'    function IsEmptyList: ' +
        'Boolean;'
      Caption = 'IsEmptyList'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 22
      OnClick = btnIsEmptyListClick
    end
    object btnN: TButton
      Left = 328
      Top = 224
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// renvoie l'#39#233'l'#233'ment N  [N] (propri'#233't'#233' par d'#233'faut)'
      Caption = '[N]'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 23
      OnClick = btnNClick
    end
    object btnCount: TButton
      Left = 16
      Top = 264
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// compte les '#233'l'#233'ments de la liste'#13#10'  function Count: Integer;'
      Caption = 'Count'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 24
      OnClick = btnCountClick
    end
    object btnLastItem: TButton
      Left = 120
      Top = 264
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// dernier '#233'l'#233'ment trait'#233#13#10'    property LastItem: Integer read f' +
        'NumLastItem default -1;'
      Caption = 'LastItem'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 25
      OnClick = btnLastItemClick
    end
    object btnLastErrorPos: TButton
      Left = 224
      Top = 264
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// position de la derni'#232're erreur'#13#10'    property LastErrorPos: In' +
        'teger read GetLastErrorPos default -1;'
      Caption = 'LastErrorPos'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 26
      OnClick = btnLastErrorPosClick
    end
    object btnRotate: TButton
      Left = 328
      Top = 264
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Hint = '// rotation de la liste'#13#10'    function Rotate: string;'
      Caption = 'Rotate'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 27
      OnClick = btnRotateClick
    end
  end
end
