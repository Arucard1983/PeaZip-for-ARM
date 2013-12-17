unit Unit6; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, Buttons;

type

  { TFormInput }

  TFormInput = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    Editinputquery: TEdit;
    Labelinputquery: TLabel;
    OpenDialog3: TOpenDialog;
    procedure BitBtn2Click(Sender: TObject);
    procedure EditinputqueryKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormInput: TFormInput;

implementation

{ TFormInput }

procedure TFormInput.EditinputqueryKeyPress(Sender: TObject; var Key: char);
begin
if Key=char(13) then
   begin
   FormInput.Close;
   FormInput.ModalResult:=1;
   end;
end;

procedure TFormInput.BitBtn2Click(Sender: TObject);
begin
if FormInput.OpenDialog3.Execute then
   if FormInput.OpenDialog3.FileName<>'' then
      begin
      {$IFDEF MSWINDOWS}
      FormInput.Editinputquery.Caption:='"'+FormInput.OpenDialog3.FileName+'"';
      {$ELSE}
      FormInput.Editinputquery.Caption:=''''+FormInput.OpenDialog3.FileName+'''';
      {$ENDIF}
      end;
end;

initialization
  {$I unit6.lrs}

end.

