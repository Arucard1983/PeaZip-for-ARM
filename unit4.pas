unit Unit4; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    MemoAbout: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormAbout: TFormAbout;

implementation

initialization
  {$I unit4.lrs}

end.

