unit Unit11;

{$mode objfpc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, activex, ComObj,
  {$ENDIF}
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TFormDrop }

  TFormDrop = class(TForm)
    Imagedragfile: TImage;
    Imagedragfolder: TImage;
    Imagedragop: TImage;
    Labeldragfile: TLabel;
    Labeldragfolder: TLabel;
    Labeldragtitle: TLabel;
    PanelDrag: TPanel;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  {$IFDEF MSWINDOWS}
  type //used for transparence
     TSetLayeredWindowAttributes = function(hwnd: HWND; crKey: COLORREF; bAlpha: Byte; dwFlags: Longint): Longint;
     stdcall;

  type //used for transparence
     TSHMultiFileProperties = function(pDataObj: IDataObject; Flag: DWORD): HRESULT;
     stdcall;
  {$ENDIF}

var
  FormDrop: TFormDrop;
  {$IFDEF MSWINDOWS}
   //used for transparence
   hUser32: HMODULE;
   SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
   SHMultiFileProperties: TSHMultiFileProperties;
   osVerInfo: TOSVersionInfo;
   {$ENDIF}

implementation

{ TFormDrop }

procedure setmswindowsopacity;
begin
{$IFDEF MSWINDOWS}
osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
if GetVersionEx(osVerInfo) then
   begin
   if osVerInfo.dwMajorVersion>4 then //the system is NTx and most pmRecent than NT4
      begin
      {following code make problems on Win9x: while it correctly avoid importing
      user32.dll if SetLayeredWindowAttributes is not supported, it raises randomic
      errors on 9x systems, so the code is made unreachable for those systems and
      for NT4 that will not support that function anyway}
         try
            hUser32 := GetModuleHandle(PChar('USER32.DLL'));
            if hUser32 <> 0 then
               begin
               pointer(SetLayeredWindowAttributes) := GetProcAddress(hUser32, 'SetLayeredWindowAttributes');
               if @SetLayeredWindowAttributes <> nil then
                  begin
                  SetWindowLongPtr(FormDrop.Handle, GWL_EXSTYLE, GetWindowLong(FormDrop.Handle, GWL_EXSTYLE) OR WS_EX_LAYERED);
                  SetLayeredWindowAttributes(FormDrop.Handle, 0, 128, LWA_ALPHA);//not supported on 98 and NT4, called dynamically since has no meaning checking it at compile time
                  end;
               end;
         except
         end;
      end;
   end;
{$ENDIF}
end;

procedure TFormDrop.FormCreate(Sender: TObject);
begin
setmswindowsopacity;
end;

initialization
  {$I unit11.lrs}

end.

