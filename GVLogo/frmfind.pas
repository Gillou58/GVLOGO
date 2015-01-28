unit FrmFind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons,
  SynEditTypes; // types pour l'édteur

type

  { TFindForm }

  TFindForm = class(TForm)
    btnCancel: TBitBtn;
    btnFind: TBitBtn;
    cboxCase: TCheckBox;
    cboxReplace: TCheckBox;
    cbPrompt: TCheckBox;
    cbWholeWord: TCheckBox;
    cbFind: TComboBox;
    cbReplace: TComboBox;
    lblReplace: TLabel;
    lblFind: TLabel;
    rgOptions: TCheckGroup;
    rbSelected: TRadioButton;
    rbBackward: TRadioButton;
    rbForward: TRadioButton;
    rgHeading: TRadioGroup;
    rbGlobal: TRadioButton;
    rgWhere: TRadioGroup;
    rbCaret: TRadioButton;
    rbBegin: TRadioButton;
    rgBegin: TRadioGroup;
    procedure btnCancelClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure cbFindChange(Sender: TObject);
    procedure cboxCaseChange(Sender: TObject);
    procedure cboxReplaceClick(Sender: TObject);
    procedure cbPromptChange(Sender: TObject);
    procedure cbWholeWordChange(Sender: TObject);
    procedure rbBackwardChange(Sender: TObject);
    procedure rbCaretChange(Sender: TObject);
    procedure rbSelectedChange(Sender: TObject);
  private
    fSynSearch: TSynSearchOptions;
    fStart: TPoint;
    fFind, fReplace: string;
  public
    //property FindCase: Boolean read fCase write fCase;
    procedure Find;
  end;

var
  FindForm: TFindForm;

implementation

uses
  GVLogoConsts, // constantes de GVLOGO
  FrmInfo, // boîte d'information
  FrmEditor; // fenêtre d'édition

{$R *.lfm}

{ TFindForm }

procedure TFindForm.btnCancelClick(Sender: TObject);
// *** abandon de la recherche ***
begin
  Close; // on ferme la fenêtre
end;

procedure TFindForm.btnFindClick(Sender: TObject);
begin

  // non trouvée ?
  if EditorForm.SynEditEditor.SearchReplaceEx(fFind, fReplace, fSynSearch,
    fStart) = 0 then
      ShowInfoForm(Format(CrsNotFound, [fFind]));
  // position en cours
  fStart.x := EditorForm.SynEditEditor.CaretX;
  fStart.y := EditorForm.SynEditEditor.CaretY;
end;

procedure TFindForm.cbFindChange(Sender: TObject);
// *** texte à chercher ***
begin
  fFind := cbFind.Text;
end;

procedure TFindForm.cboxCaseChange(Sender: TObject);
// *** changement de casse ***
begin
  if cboxCase.Checked then
    fSynSearch := fSynSearch + [ssoMatchCase]
  else
    fSynSearch := fSynSearch - [ssoMatchCase];
end;

procedure TFindForm.cboxReplaceClick(Sender: TObject);
// *** remplacement actif ou non ***
begin
  // contrôles activés/désactivés
  lblReplace.Enabled := cboxReplace.Checked;
  cbReplace.Enabled := cboxReplace.Checked;
  cbPrompt.Enabled := cboxReplace.Checked;
  if cboxReplace.Checked then
    fSynSearch := fSynSearch + [ssoReplace]
  else
    fSynSearch := fSynSearch - [ssoReplace];
end;

procedure TFindForm.cbPromptChange(Sender: TObject);
// *** demande sur remplacement ***
begin
  if cbPrompt.Checked then
    fSynSearch := fSynSearch + [ssoPrompt]
  else
    fSynSearch := fSynSearch - [ssoPrompt];
end;

procedure TFindForm.cbWholeWordChange(Sender: TObject);
// *** mot entier ou non ***
begin
  if cbWholeWord.Checked then
    fSynSearch := fSynSearch + [ssoWholeWord]
  else
    fSynSearch := fSynSearch - [ssoWholeWord];
end;

procedure TFindForm.rbBackwardChange(Sender: TObject);
// *** en arrière ou en avant ***
begin
  if rbBackward.Checked then
     fSynSearch := fSynSearch + [ssoBackwards]
   else
     fSynSearch := fSynSearch + [ssoBackwards];
end;

procedure TFindForm.rbCaretChange(Sender: TObject);
// *** origine ou position du curseur ***
begin
  if not rbCaret.Checked then
  begin
    // début du texte
    fStart.x := 1;
    fStart.y := 1;
  end
  else
  begin
    // position en cours
    fStart.x := EditorForm.SynEditEditor.CaretX;
    fStart.y := EditorForm.SynEditEditor.CaretY;
  end;
end;

procedure TFindForm.rbSelectedChange(Sender: TObject);
// *** opération sur texte sélectionné ou globale ***
begin
  if rbSelected.Checked then
    fSynSearch := fSynSearch + [ssoSelectedOnly] - [ssoEntireScope]
  else
    fSynSearch := fSynSearch + [ssoEntireScope] - [ssoSelectedOnly];
end;

procedure TFindForm.Find;
// *** recherche ***
begin
  ShowOnTop; // fenêtre montrée
end;


end.

