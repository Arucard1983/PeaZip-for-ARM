unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ExtCtrls;

type

  { TFormAdvf }

  TFormAdvf = class(TForm)
    ButtonClearFilters: TButton;
    ButtonPanel1: TButtonPanel;
    CheckBoxAdvFilters: TCheckBox;
    CheckBoxAdvRecurse: TCheckBox;
    CheckBoxAdvRecurse1: TCheckBox;
    GroupBoxOpenFilters: TGroupBox;
    ImageInfoArchive4: TImage;
    LabelAdvExclude: TLabel;
    LabelAdvInclude: TLabel;
    MemoAdvExclude: TMemo;
    MemoAdvInclude: TMemo;
    procedure ButtonClearFiltersClick(Sender: TObject);
    procedure CheckBoxAdvFiltersClick(Sender: TObject);
    procedure ImageInfoArchive4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormAdvf: TFormAdvf;

implementation

{ TFormAdvf }

procedure TFormAdvf.CheckBoxAdvFiltersClick(Sender: TObject);
begin
if CheckBoxAdvFilters.state=cbChecked then GroupBoxOpenFilters.Enabled:=true
else
  begin
  GroupBoxOpenFilters.Enabled:=false;
  FormAdvf.Close;
  FormAdvf.ModalResult:=mrCancel;
  end;
end;

procedure TFormAdvf.ImageInfoArchive4Click(Sender: TObject);
begin
MessageDlg(ImageInfoArchive4.Hint, mtInformation, [mbOK], 0);
end;

procedure TFormAdvf.ButtonClearFiltersClick(Sender: TObject);
begin
MemoAdvInclude.Clear;
MemoAdvExclude.Clear;
end;

initialization
  {$I unit1.lrs}

end.

