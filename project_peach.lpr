program project_peach;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here },
  peach, Unit3, Unit4, Unit5, Unit6, Unit2, Unit1, Unit7, Unit8, Unit9, Unit10,
  Unit11;

{$IFDEF MSWINDOWS}
{$R peazip.res}
{$R manifest.res}
{$ENDIF}

{$R *.res}

begin
Application.Title:='PeaZip';
Application.Initialize;
Application.CreateForm(TForm_peach, Form_peach);
  Application.CreateForm(TFormCrop, FormCrop);
  Application.CreateForm(TFormDrop, FormDrop);
Application.Run;
end.

