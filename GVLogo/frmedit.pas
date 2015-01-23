unit FrmEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TEditForm }

  TEditForm = class(TForm)
    EditCmdLine: TEdit;
    procedure EditCmdLineKeyPress(Sender: TObject; var Key: char);
  private
  public
  end;

var
  EditForm: TEditForm;

implementation

uses
  Main; // fiche principale

{$R *.lfm}

{ TEditForm }

procedure TEditForm.EditCmdLineKeyPress(Sender: TObject; var Key: char);
// *** test de la touche entrée ***
begin
  if Key = #13 then
    MainForm.ExecExecuteExecute(nil); // exécution des commandes
end;

end.

