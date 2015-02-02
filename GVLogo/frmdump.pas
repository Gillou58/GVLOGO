unit FrmDump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  GVHighlighter; // coloration syntaxique

type

  { TDumpForm }

  TDumpForm = class(TForm)
    SynEditDump: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fGVHighlighter: TGVHighlighter; // coloration syntaxique
  public
    procedure Dump; // affichage
  end;

var
  DumpForm: TDumpForm;

implementation

{$R *.lfm}

uses
  Main; // fiche principale

{ TDumpForm }

procedure TDumpForm.FormCreate(Sender: TObject);
// *** création de la fenêtre ***
begin
  fGVHighlighter := TGVHighlighter.Create(Self); // création de la colorisation
  SynEditDump.Highlighter := fGVHighlighter; // éditeur lié
  SynEditDump.Lines.Clear; // nettoyage
end;

procedure TDumpForm.FormDestroy(Sender: TObject);
// *** destruction de la fenêtre ***
begin
  fGVHighlighter.Free; // coloration libérée
end;

procedure TDumpForm.Dump;
// *** affichage du contenu du noyau ***
var
  Lst: TStringList;
  LS: string;
begin
  Lst := TStringList.Create; // création de la fiche
  try
    MainForm.Automat.Kernel.Dump(Lst); // recherche du contenu
    SynEditDump.Lines.Clear; // éditeur nettoyé
    for LS in Lst do // on balaie la liste
      SynEditDump.Lines.Add(LS);
  finally
    Lst.Free; // libération de la liste
  end;
end;

end.

