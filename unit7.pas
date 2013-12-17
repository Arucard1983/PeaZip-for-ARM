unit Unit7; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Grids, ButtonPanel, Buttons, Menus;

type

  { TFormApps }

  TFormApps = class(TForm)
    ButtonPanel1: TButtonPanel;
    Imagece1: TImage;
    Imagece2: TImage;
    Labelcb11: TBitBtn;
    Labelcb15: TLabel;
    Labelcb6: TLabel;
    Labelcb8: TLabel;
    Labelcb9: TLabel;
    OpenDialog1: TOpenDialog;
    PanelApps: TPanel;
    pmCustEdit: TPopupMenu;
    pmCustEdit1: TPopupMenu;
    pmCustEditAdd: TMenuItem;
    pmCustEditAdd1: TMenuItem;
    pmCustEditDesc: TMenuItem;
    pmCustEditDesc1: TMenuItem;
    pmCustEditEdit: TMenuItem;
    pmCustEditEdit1: TMenuItem;
    pmCustEditEdit1after: TMenuItem;
    pmCustEditRemove: TMenuItem;
    pmCustEditRemove1: TMenuItem;
    StringGridCustEdit: TStringGrid;
    StringGridCustEdit1: TStringGrid;
    procedure pmCustEditAdd1Click(Sender: TObject);
    procedure pmCustEditAddClick(Sender: TObject);
    procedure pmCustEditDesc1Click(Sender: TObject);
    procedure pmCustEditDescClick(Sender: TObject);
    procedure pmCustEditEdit1afterClick(Sender: TObject);
    procedure pmCustEditEdit1Click(Sender: TObject);
    procedure pmCustEditEditClick(Sender: TObject);
    procedure pmCustEditRemove1Click(Sender: TObject);
    procedure pmCustEditRemoveClick(Sender: TObject);
    procedure StringGridCustEdit1ColRowMoved(Sender: TObject;
      IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure StringGridCustEdit1DblClick(Sender: TObject);
    procedure StringGridCustEdit1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StringGridCustEdit1SelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure StringGridCustEditColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure StringGridCustEditDblClick(Sender: TObject);
    procedure StringGridCustEditMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure set_adveditcl(i:integer);

var
  FormApps: TFormApps;
  txt_description,txt_2_5_strbefore,txt_2_5_strafter,txt_3_3_stralt,txt_edit,delimiter,
  advedit1before,advedit2before,advedit3before,advedit4before,advedit5before,advedit6before,advedit7before,advedit8before,
  advedit1after,advedit2after,advedit3after,advedit4after,advedit5after,advedit6after,advedit7after,advedit8after:utf8string;

implementation

{ TFormApps }

procedure set_adveditcl(i:integer);
begin
case i of
   1: FormApps.Labelcb15.caption:=advedit1before+delimiter+'%f'+delimiter+advedit1after;
   2: FormApps.Labelcb15.caption:=advedit2before+delimiter+'%f'+delimiter+advedit2after;
   3: FormApps.Labelcb15.caption:=advedit3before+delimiter+'%f'+delimiter+advedit3after;
   4: FormApps.Labelcb15.caption:=advedit4before+delimiter+'%f'+delimiter+advedit4after;
   5: FormApps.Labelcb15.caption:=advedit5before+delimiter+'%f'+delimiter+advedit5after;
   6: FormApps.Labelcb15.caption:=advedit6before+delimiter+'%f'+delimiter+advedit6after;
   7: FormApps.Labelcb15.caption:=advedit7before+delimiter+'%f'+delimiter+advedit7after;
   8: FormApps.Labelcb15.caption:=advedit8before+delimiter+'%f'+delimiter+advedit8after;
   end;
end;

procedure setdescription_advcustedit;
var
   s:utf8string;
begin
s:=FormApps.StringGridCustedit1.Cells[1,FormApps.StringGridCustedit1.Row];
if InputQuery(txt_description, '', s) then
   begin
   FormApps.StringGridCustedit1.Cells[1,FormApps.StringGridCustedit1.Row]:=s;
   FormApps.StringGridCustedit1.AutoSizeColumns;
   end;
set_adveditcl(FormApps.StringGridCustedit1.Row);
end;

procedure setdescription_custedit;
var
   s:utf8string;
begin
s:=FormApps.StringGridCustedit.Cells[1,FormApps.StringGridCustedit.Row];
if InputQuery(txt_description, '', s) then
   begin
   FormApps.StringGridCustedit.Cells[1,FormApps.StringGridCustedit.Row]:=s;
   FormApps.StringGridCustedit.AutoSizeColumns;
   end;
end;

procedure editstring1_advcustedit;
var
   s:utf8string;
begin
s:=FormApps.StringGridCustedit1.Cells[2,FormApps.StringGridCustedit1.Row];
if InputQuery(txt_2_5_strbefore, '', s) then
   begin
   FormApps.StringGridCustedit1.Cells[2,FormApps.StringGridCustedit1.Row]:=s;
   FormApps.StringGridCustedit1.AutoSizeColumns;
   end;
set_adveditcl(FormApps.StringGridCustedit1.Row);
end;

procedure editstring2_advcustedit;
var
   s:utf8string;
begin
s:=FormApps.StringGridCustedit1.Cells[3,FormApps.StringGridCustedit1.Row];
if InputQuery(txt_2_5_strafter, '', s) then
   begin
   FormApps.StringGridCustedit1.Cells[3,FormApps.StringGridCustedit1.Row]:=s;
   FormApps.StringGridCustedit1.AutoSizeColumns;
   end;
set_adveditcl(FormApps.StringGridCustedit1.Row);
end;

procedure editstring3_advcustedit;
var
   s:utf8string;
begin
s:=FormApps.StringGridCustedit1.Cells[4,FormApps.StringGridCustedit1.Row];
if InputQuery(txt_3_3_stralt, '', s) then
   begin
   FormApps.StringGridCustedit1.Cells[4,FormApps.StringGridCustedit1.Row]:=s;
   FormApps.StringGridCustedit1.AutoSizeColumns;
   end;
set_adveditcl(FormApps.StringGridCustedit1.Row);
end;

procedure editapp_custedit;
var
   s:utf8string;
begin
s:=FormApps.StringGridCustedit.Cells[2,FormApps.StringGridCustedit.Row];
if InputQuery(txt_edit, '', s) then
   begin
   FormApps.StringGridCustedit.Cells[2,FormApps.StringGridCustedit.Row]:=s;
   FormApps.StringGridCustedit.AutoSizeColumns;
   end;
end;

procedure TFormApps.StringGridCusteditColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
var
   i:integer;
begin
for i:=1 to 16 do FormApps.StringGridCustedit.Cells[0,i]:=inttostr(i);
end;

procedure TFormApps.StringGridCustEditDblClick(Sender: TObject);
begin
case FormApps.StringGridCustedit.Col of
   1: setdescription_custedit;
   2: editapp_custedit;
   end;
end;

procedure TFormApps.StringGridCustEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   Column,Row:integer;
begin
FormApps.StringGridCustedit.MouseToCell(X, Y, Column, Row);
FormApps.StringGridCustedit.Row:=Row;
end;

procedure TFormApps.StringGridCustEdit1ColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
var
   i:integer;
begin
for i:=1 to 8 do FormApps.StringGridCustedit1.Cells[0,i]:=inttostr(i);
end;

procedure TFormApps.StringGridCustEdit1DblClick(Sender: TObject);
begin
case FormApps.StringGridCustedit1.Col of
   1: setdescription_advcustedit;
   2: editstring1_advcustedit;
   3: editstring2_advcustedit;
   4: editstring3_advcustedit;
   end;
end;

procedure TFormApps.StringGridCustEdit1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   Column,Row:integer;
begin
FormApps.StringGridCustedit1.MouseToCell(X, Y, Column, Row);
FormApps.StringGridCustedit1.Row:=Row;
set_adveditcl(Row);
end;

procedure TFormApps.StringGridCustEdit1SelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
set_adveditcl(aRow);
end;

procedure TFormApps.pmCustEditAdd1Click(Sender: TObject);
begin
if OpenDialog1.Execute then
   if OpenDialog1.Filename<>'' then
      begin
      FormApps.StringGridCustedit1.Cells[2,FormApps.StringGridCustedit1.Row]:=OpenDialog1.Filename;
      FormApps.StringGridCustedit1.AutoSizeColumns;
      end
   else exit
else exit;
set_adveditcl(FormApps.StringGridCustedit1.Row);
end;

procedure TFormApps.pmCustEditAddClick(Sender: TObject);
begin
if OpenDialog1.Execute then
   if OpenDialog1.Filename<>'' then
      begin
      FormApps.StringGridCustedit.Cells[2,FormApps.StringGridCustedit.Row]:=OpenDialog1.Filename;
      FormApps.StringGridCustedit.AutoSizeColumns;
      end
   else exit
else exit;
end;

procedure TFormApps.pmCustEditDesc1Click(Sender: TObject);
begin
setdescription_advcustedit;
end;

procedure TFormApps.pmCustEditDescClick(Sender: TObject);
begin
setdescription_custedit;
end;

procedure TFormApps.pmCustEditEdit1Click(Sender: TObject);
begin
editstring1_advcustedit;
end;

procedure TFormApps.pmCustEditEditClick(Sender: TObject);
begin
editapp_custedit;
end;

procedure TFormApps.pmCustEditRemove1Click(Sender: TObject);
begin
FormApps.StringGridCustedit1.Cells[1,FormApps.StringGridCustedit1.Row]:='';
FormApps.StringGridCustedit1.Cells[2,FormApps.StringGridCustedit1.Row]:='';
FormApps.StringGridCustedit1.Cells[3,FormApps.StringGridCustedit1.Row]:='';
FormApps.StringGridCustedit1.Cells[4,FormApps.StringGridCustedit1.Row]:='';
FormApps.StringGridCustedit1.AutoSizeColumns;
set_adveditcl(FormApps.StringGridCustedit1.Row);
end;

procedure TFormApps.pmCustEditRemoveClick(Sender: TObject);
begin
FormApps.StringGridCustedit.Cells[1,FormApps.StringGridCustedit.Row]:='';
FormApps.StringGridCustedit.Cells[2,FormApps.StringGridCustedit.Row]:='';
FormApps.StringGridCustedit.AutoSizeColumns;
end;

procedure TFormApps.pmCustEditEdit1afterClick(Sender: TObject);
begin
editstring2_advcustedit;
end;

initialization
  {$I unit7.lrs}

end.

