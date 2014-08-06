object MainForm: TMainForm
  Left = 0
  Top = 0
  ActiveControl = edtName
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Test de GVPropLists - V1.0.0 - GVLOGO'
  ClientHeight = 579
  ClientWidth = 890
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
    Top = 8
    Width = 409
    Height = 497
    Cursor = crNo
    Hint = 'zone d'#39'affichage'
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 0
  end
  object statmain: TStatusBar
    Left = 0
    Top = 560
    Width = 890
    Height = 19
    AutoHint = True
    Panels = <>
    ParentShowHint = False
    ShowHint = False
    SimplePanel = True
  end
  object btnClear: TBitBtn
    Left = 342
    Top = 520
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Hint = 'Nettoyage de l'#39#233'diteur'
    Caption = 'Nettoyer'
    Kind = bkCancel
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = btnClearClick
  end
  object grpEdit: TGroupBox
    Left = 432
    Top = 8
    Width = 450
    Height = 161
    Caption = 'Saisie '
    TabOrder = 3
    object lblName: TLabel
      Left = 40
      Top = 32
      Width = 28
      Height = 13
      Caption = 'Nom :'
    end
    object lblProp: TLabel
      Left = 39
      Top = 64
      Width = 51
      Height = 13
      Caption = 'Propri'#233't'#233' :'
    end
    object lblValue: TLabel
      Left = 40
      Top = 96
      Width = 37
      Height = 13
      Caption = 'Valeur :'
    end
    object lblNUm: TLabel
      Left = 40
      Top = 131
      Width = 36
      Height = 13
      Caption = 'Indice :'
    end
    object edtName: TEdit
      Left = 96
      Top = 29
      Width = 329
      Height = 21
      Cursor = crHandPoint
      Hint = 'Saisissez le nom de la liste de propri'#233't'#233's.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'caniche'
    end
    object edtProp: TEdit
      Left = 96
      Top = 61
      Width = 329
      Height = 21
      Cursor = crHandPoint
      Hint = 'Saisissez le nom de la propri'#233't'#233'.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'couleur'
    end
    object edtValue: TEdit
      Left = 96
      Top = 93
      Width = 329
      Height = 21
      Cursor = crHandPoint
      Hint = 'Saisissez la valeur de la propri'#233't'#233'.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'blanc'
    end
    object seNum: TSpinEdit
      Left = 96
      Top = 128
      Width = 121
      Height = 22
      Cursor = crHandPoint
      Hint = 'Saisissez le num'#233'ro de la propri'#233't'#233'.'
      MaxValue = 50
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Value = 1
    end
  end
  object grpPropLists: TGroupBox
    Left = 432
    Top = 192
    Width = 234
    Height = 313
    Caption = 'Listes de propri'#233't'#233's'
    TabOrder = 4
    object btnClearP: TButton
      Left = 3
      Top = 48
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = '// nettoyage des listes'#13#10'  procedure Clear;'
      Caption = 'Clear'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnClearPClick
    end
    object btnUpdateListP: TButton
      Left = 123
      Top = 48
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// cr'#233'e ou met '#224' jour la liste de propri'#233't'#233's'#13#10'    function UpDat' +
        'eListP(const Name, Prop, Value: string): Boolean;'
      Caption = 'UpdateListP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnUpdateListPClick
    end
    object btnRemoveListP: TButton
      Left = 3
      Top = 79
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// destruction d'#39'une liste de propri'#233't'#233's'#13#10'    function RemoveLis' +
        'tP(const Name: string): Boolean;'
      Caption = 'RemoveListP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnRemoveListPClick
    end
    object btnCountListP: TButton
      Left = 123
      Top = 79
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie le nombre de listes de propri'#233't'#233's'#13#10'    function Count' +
        'ListP: Integer;'
      Caption = 'CountListP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btnCountListPClick
    end
    object btnIsListP: TButton
      Left = 3
      Top = 110
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// la liste existe-t-elle ?'#13#10'    function IsListP(const Name: st' +
        'ring): Boolean;'
      Caption = 'IsListP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = btnIsListPClick
    end
    object btnValListP: TButton
      Left = 123
      Top = 110
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie la valeur d'#39'une liste'#13#10'    function ValListP(const Na' +
        'me: string): string;'
      Caption = 'ValListP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnValListPClick
    end
    object btnNumListP: TButton
      Left = 3
      Top = 141
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie le num'#233'ro d'#39'une liste de propri'#233't'#233's'#13#10'    function Num' +
        'ListP(const Name: string): Integer;'
      Caption = 'NumListP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = btnNumListPClick
    end
    object btnValNumListP: TButton
      Left = 123
      Top = 141
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// valeur d'#39'une liste de propri'#233't'#233's par num'#233'ro'#13#10'    function Val' +
        'NumListP(N: Integer; out Name, Value: string): Boolean;'
      Caption = 'ValNumListP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = btnValNumListPClick
    end
    object btnListPByNum: TButton
      Left = 3
      Top = 172
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// liste de propri'#233't'#233's par num'#233'ro'#13#10'    property ListPByNum[N: In' +
        'teger]: string read GetLPByNum; default;'
      Caption = 'ListPByNum'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = btnListPByNumClick
    end
    object btnIsListPByNum: TButton
      Left = 3
      Top = 234
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// la propri'#233't'#233' N existe-t-elle?'#13#10'    function IsListPByNum(N: I' +
        'nteger): Boolean;'
      Caption = 'IsListPByNum'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = btnIsListPByNumClick
    end
    object btnLoadFromFile: TButton
      Left = 3
      Top = 203
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// chargement des listes'#13#10'    procedure LoadFromFile(const FileN' +
        'ame: string);'
      Caption = 'LoadFromFile'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = btnLoadFromFileClick
    end
    object btnSaveToFile: TButton
      Left = 123
      Top = 203
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// sauvegarde des listes'#13#10'    procedure SaveToFile(const FileNam' +
        'e: string);'
      Caption = 'SaveToFile'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = btnSaveToFileClick
    end
    object btnN: TButton
      Left = 123
      Top = 172
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// liste de propri'#233't'#233's par num'#233'ro'#13#10'    property ListPByNum[N: In' +
        'teger]: string read GetLPByNum; default;'
      Caption = '[N]'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = btnNClick
    end
    object btnListP: TButton
      Left = 123
      Top = 234
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie la liste des listes de propri'#233't'#233's'#13#10'    function ListP' +
        ': string;'
      Caption = 'ListP'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = btnListPClick
    end
    object btnEnumerator: TButton
      Left = 3
      Top = 265
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = '// '#233'num'#233'ration gr'#226'ce '#224' '#39'S in...'#39
      Caption = 'Enumerator'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = btnEnumeratorClick
    end
  end
  object grpProps: TGroupBox
    Left = 672
    Top = 192
    Width = 210
    Height = 313
    Caption = 'Propri'#233't'#233's '
    TabOrder = 5
    object btnIsProp: TButton
      Left = 3
      Top = 48
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        ' // la propri'#233't'#233' existe-t-elle ?'#13#10'    function IsProp(const Name' +
        ', Prop: string): Boolean;'
      Caption = 'IsProp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnIsPropClick
    end
    object btnNumProp: TButton
      Left = 107
      Top = 48
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie le num'#233'ro d'#39'une propri'#233't'#233#13#10'    function NumProp(const' +
        ' Name, Prop: string): Integer;'
      Caption = 'NumProp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnNumPropClick
    end
    object btnValProp: TButton
      Left = 3
      Top = 79
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// valeur d'#39'une propri'#233't'#233#13#10'    function ValProp(const Name, Prop' +
        ': string): string; overload;'#13#10'    function ValProp(const Name, P' +
        'rop: string; out Value: string)'#13#10'      : Boolean; overload;'
      Caption = 'ValProp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnValPropClick
    end
    object btnRemoveProp: TButton
      Left = 107
      Top = 79
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// destruction d'#39'une propri'#233't'#233#13#10'    function RemoveProp(const Na' +
        'me, Prop: string): Boolean;'
      Caption = 'RemoveProp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btnRemovePropClick
    end
    object btnCountProps: TButton
      Left = 3
      Top = 110
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// renvoie le nombre de propri'#233't'#233's attach'#233'es '#224' une liste'#13#10'    fu' +
        'nction CountProps(const Name: string): Integer;'
      Caption = 'CountProps'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = btnCountPropsClick
    end
    object btnValNumProp: TButton
      Left = 107
      Top = 110
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// valeur d'#39'une propri'#233't'#233' par num'#233'ro'#13#10'    function ValNumProp(co' +
        'nst Name: string; N: Integer; out Prop: string)'#13#10'      : Boolean' +
        '; overload;'#13#10'    function ValNumProp(const Name: string; N: Inte' +
        'ger): string; overload;'
      Caption = 'ValNumProp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnValNumPropClick
    end
    object btnListOfProps: TButton
      Left = 3
      Top = 141
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        ' // liste des propri'#233't'#233's d'#39'une liste'#13#10'    function ListOfProps(c' +
        'onst Name: string): string;'
      Caption = 'ListOfProps'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = btnListOfPropsClick
    end
    object btnNameOfProp: TButton
      Left = 107
      Top = 141
      Width = 100
      Height = 25
      Cursor = crHandPoint
      Hint = 
        '// nom d'#39'une propri'#233't'#233' par num'#233'ro'#13#10'    function NameOfProp(const' +
        ' Name: string; N: Integer): string; overload;'#13#10'    function Name' +
        'OfProp(const Name: string; N: Integer; out Prop: string)'#13#10'      ' +
        ': Boolean; overload;'
      Caption = 'NameOfProp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = btnNameOfPropClick
    end
  end
end
