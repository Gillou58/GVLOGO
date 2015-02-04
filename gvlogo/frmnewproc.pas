unit FrmNewProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, Buttons,
   GVHighlighter; // coloration syntaxique

type
  // *** TNewProcForm ***

  { TNewProcForm }

  TNewProcForm = class(TForm)
    btnClose: TBitBtn;
    btnClear: TBitBtn;
    btnSave: TBitBtn;
    lbledtParams: TLabeledEdit;
    lbledtName: TLabeledEdit;
    pnlNewProc2: TPanel;
    pnlNewProc: TPanel;
    sbNewProc: TStatusBar;
    SynEditNewProc: TSynEdit;
    procedure btnClearClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEditNewProcChange(Sender: TObject);
  private
    fGVHighlighter: TGVHighlighter; // coloration syntaxique
  public
  end;

  function ShowNewProcForm: TModalResult; // affichage

implementation

{$R *.lfm}

uses
  FrmInfo, // boîtes d'information
  GVLOGOConsts; // constantes du projet

function ShowNewProcForm: TModalResult;
// *** affiche la fiche ***
var
  GVNewProcForm: TNewProcForm;
begin
  GVNewProcForm := TNewProcForm.Create(nil); // fiche créée
  try
    Result := GVNewProcForm.ShowModal; // fiche affichée
  finally
    GVNewProcForm.Free; // fiche libérée
  end;
end;

{ TNewProcForm }

procedure TNewProcForm.SynEditNewProcChange(Sender: TObject);
// *** éditeur modifié ***
begin
  // message suivant l'état du texte
  if SynEditNewProc.Modified then
    sbNewProc.SimpleText := CrsModified
  else
    sbNewProc.SimpleText := CrsOk;
  // bouton de sauvegarde adapté à l'état du texte
  btnSave.Enabled := SynEditNewProc.Modified;
end;

procedure TNewProcForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
begin
  fGVHighlighter := TGVHighlighter.Create(Self); // création de la colorisation
  SynEditNewProc.Highlighter := fGVHighlighter; // éditeur lié
  SynEditNewProc.Lines.Clear; // nettoyage
  btnSave.Enabled := False; // sauvegarde désactivée
end;

procedure TNewProcForm.btnClearClick(Sender: TObject);
// *** nettoyage de la fenêtre ***
begin
  SynEditNewProc.Lines.Clear;
  lbledtName.Clear;
  lbledtParams.Clear;
end;

procedure TNewProcForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
// *** fermeture de la fenêtre ***
begin
  if SynEditNewProc.Modified then // éditeur modifié ?
  begin
    // confirmation
    case FrmInfo.ShowConfirmForm(Format(CrsSave,
      [lbledtName.Text])) of
        // on veut enregistrer la procédure
        mrYes: CanClose := False; // ### enregistrer TODO ###
        mrNo: CanClose := True; // pas d'enregistrement
        mrCancel: CanClose := False; // abandon
      end;
  end
  else
    CanClose := True; // on ferme la fenêtre
end;

procedure TNewProcForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  fGVHighlighter.Free; // libération de la colorisation
end;

end.

