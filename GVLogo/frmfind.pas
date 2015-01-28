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
    cboxPrompt: TCheckBox;
    cboxWholeWord: TCheckBox;
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
    procedure btnCancelClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure cbFindChange(Sender: TObject);
    procedure cboxCaseChange(Sender: TObject);
    procedure cboxReplaceChange(Sender: TObject);
    procedure cboxPromptChange(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure cboxWholeWordChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure rbBackwardChange(Sender: TObject);
    procedure rbSelectedChange(Sender: TObject);
  private
    fSynSearch: TSynSearchOptions;
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
// *** bouton de recherche ***
begin
  // non trouvée ?
  if EditorForm.SynEditEditor.SearchReplace(fFind, fReplace,
    fSynSearch) = 0 then
  begin
    ShowInfoForm(Format(CrsNotFound, [fFind])); // on informe
    fSynSearch := fSynSearch - [ssoFindContinue]; // on ne continue pas
  end
  else
    fSynSearch := fSynSearch + [ssoFindContinue]; // recherche poursuivie
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

procedure TFindForm.cboxReplaceChange(Sender: TObject);
// *** remplacement actif ou non ***
begin
  // contrôles activés/désactivés
  lblReplace.Enabled := cboxReplace.Checked;
  cbReplace.Enabled := cboxReplace.Checked;
  cboxPrompt.Enabled := cboxReplace.Checked;
  if cboxReplace.Checked then
    fSynSearch := fSynSearch + [ssoReplace]
  else
    fSynSearch := fSynSearch - [ssoReplace];
end;

procedure TFindForm.cboxPromptChange(Sender: TObject);
// *** demande sur remplacement ***
begin
  if cboxPrompt.Checked then
    fSynSearch := fSynSearch + [ssoPrompt]
  else
    fSynSearch := fSynSearch - [ssoPrompt];
end;

procedure TFindForm.cbReplaceChange(Sender: TObject);
// *** texte de remplacement ***
begin
  fReplace := cbReplace.Text;
end;

procedure TFindForm.cboxWholeWordChange(Sender: TObject);
// *** mot entier ou non ***
begin
  if cboxWholeWord.Checked then
    fSynSearch := fSynSearch + [ssoWholeWord]
  else
    fSynSearch := fSynSearch - [ssoWholeWord];
end;

procedure TFindForm.FormActivate(Sender: TObject);
// *** activation de la fenêtre ***
begin
  // mise à jour des contrôles
  cbFind.Text := fFind;
  cboxCase.Checked := (ssoMatchCase in fSynSearch);
  cboxWholeWord.Checked := (ssoWholeWord in fSynSearch);
  cboxPrompt.Checked := (ssoPrompt in fSynSearch);
  rbSelected.Checked := (ssoSelectedOnly in fSynSearch);
  rbBackward.Checked := (ssoBackwards in fSynSearch);
  fSynSearch := fSynSearch - [ssoFindContinue];
end;

procedure TFindForm.rbBackwardChange(Sender: TObject);
// *** en arrière ou en avant ***
begin
  if rbBackward.Checked then
     fSynSearch := fSynSearch + [ssoBackwards]
   else
     fSynSearch := fSynSearch - [ssoBackwards];
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

