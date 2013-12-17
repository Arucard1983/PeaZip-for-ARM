unit Unit10; 

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, ButtonPanel, StdCtrls, Spin;

type

  { TFormCrop }

  TFormCrop = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBoxPercent: TCheckBox;
    ImageInfoArchive4: TImage;
    LabelL: TLabel;
    LabelT: TLabel;
    LabelB: TLabel;
    LabelR: TLabel;
    SpinEditL: TSpinEdit;
    SpinEditT: TSpinEdit;
    SpinEditB: TSpinEdit;
    SpinEditR: TSpinEdit;
    procedure CheckBoxPercentChange(Sender: TObject);
    procedure ImageInfoArchive4Click(Sender: TObject);
    procedure SpinEditLChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormCrop: TFormCrop;

implementation

{ TFormCrop }

{ TFormCrop }

procedure TFormCrop.CheckBoxPercentChange(Sender: TObject);
begin
  if CheckBoxPercent.Checked=true then
     begin
     SpinEditL.Value:=10;
     SpinEditR.Value:=10;
     SpinEditT.Value:=10;
     SpinEditB.Value:=10;
     end
  else
     begin
     SpinEditL.Value:=300;
     SpinEditR.Value:=300;
     SpinEditT.Value:=300;
     SpinEditB.Value:=300;
     end;
end;

procedure TFormCrop.ImageInfoArchive4Click(Sender: TObject);
begin
MessageDlg(ImageInfoArchive4.Hint, mtInformation, [mbOK], 0);
end;

procedure percentsetcrop(dim:integer);
begin
with FormCrop do
begin
if CheckBoxPercent.Checked=true then
   case dim of
   1: if SpinEditL.Value+SpinEditR.Value>=100 then SpinEditR.Value:=99-SpinEditL.Value;
   end;
end;
end;

procedure TFormCrop.SpinEditLChange(Sender: TObject);
begin
percentsetcrop(1);
end;

initialization
  {$I unit10.lrs}

end.

