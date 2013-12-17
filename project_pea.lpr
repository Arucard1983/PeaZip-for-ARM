program project_pea;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, 
  Unit_pea, pea_utils, list_utils, Unit_report;

{$IFDEF MSWINDOWS}
{$R pea.res}
{$R manifest.res}
{$ENDIF}

{$R *.res}

begin
  Application.Title:='Pea';
  Application.Initialize;
  Application.CreateForm(TForm_pea, Form_pea);
  //Application.CreateForm(TForm_report, Form_report);
  Application.Run;
end.

