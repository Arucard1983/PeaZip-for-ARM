program project_gwrap;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Unit_gwrap;
  
{$IFDEF MSWINDOWS}
{$R pealauncher.res}
{$R manifest.res}
{$ENDIF}

{$R *.res}

begin
  Application.Title:='PeaLauncher';
  Application.Initialize;
  Application.CreateForm(TForm_gwrap, Form_gwrap);
  Application.Run;
end.

