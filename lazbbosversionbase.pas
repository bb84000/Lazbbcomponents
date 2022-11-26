{*******************************************************************************
 lazbbosversionbase : Base functions and defintionsfor lazbbOsVersion component
 sdtp - bb - september 2022
********************************************************************************}
unit lazbbosversionbase;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS}
    Windows,
  {$ELSE}
    process,
  {$ENDIF}  Classes, SysUtils;



  {$IFDEF WINDOWS}
   type
    // missing structure for GetVersionEx Windows function
    POSVersionInfoExA = ^TOSVersionInfoExA;
    POSVersionInfoExW = ^TOSVersionInfoExW;
    POSVersionInfoEx = POSVersionInfoExA;
    _OSVERSIONINFOEXA = record
      dwOSVersionInfoSize: DWORD;
      dwMajorVersion: DWORD;
      dwMinorVersion: DWORD;
      dwBuildNumber: DWORD;
      dwPlatformId: DWORD;
      szCSDVersion: array[0..127] of AnsiChar; { Maintenance string for PSS usage }
      wServicePackMajor: Word;
      wServicePackMinor: Word;
      wSuiteMask: WORD;
      wProductType: BYTE;
      wReserved: BYTE;
    end;
    _OSVERSIONINFOEXW = record
      dwOSVersionInfoSize: DWORD;
      dwMajorVersion: DWORD;
      dwMinorVersion: DWORD;
      dwBuildNumber: DWORD;
      dwPlatformId: DWORD;
      szCSDVersion: array[0..127] of WideChar; { Maintenance string for PSS usage }
      wServicePackMajor: Word;
      wServicePackMinor: Word;
      wSuiteMask: WORD;
      wProductType: BYTE;
      wReserved: BYTE;
    end;
    _OSVERSIONINFOEX = _OSVERSIONINFOEXA;
     TOSVersionInfoExA = _OSVERSIONINFOEXA;
    TOSVersionInfoExW = _OSVERSIONINFOEXW;
    TOSVersionInfoEx = TOSVersionInfoExA;
    OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
    OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
    OSVERSIONINFOEX = OSVERSIONINFOEXA;

const
    // Winbdows constants
    VER_NT_WORKSTATION       = 1;
    VER_NT_DOMAIN_CONTROLLER = 2;
    VER_NT_SERVER            = 3;
    VER_SUITE_SMALLBUSINESS            = $0001;
    VER_SUITE_ENTERPRISE               = $0002;
    VER_SUITE_BACKOFFICE               = $0004;
    VER_SUITE_TERMINAL                 = $0010;
    VER_SUITE_SMALLBUSINESS_RESTRICTED = $0020;
    VER_SUITE_DATACENTER               = $0080;
    VER_SUITE_PERSONAL                 = $0200;

var
  //Windows functions to be loaded dynamlically
  hProductInfo: THandle;
  // Exists in Vista and over
  GetProductInfo: function (dwOSMajorVersion, dwOSMinorVersion,
                            dwSpMajorVersion, dwSpMinorVersion: DWORD;
                            var pdwReturnedProductType: DWORD): BOOL stdcall = NIL;
  // Exists in W2000 and over
  GetVersionEx: function (var lpVersionInformation: TOSVersionInfoEx): BOOL stdcall= NIL;
  {$ENDIF}

implementation

initialization
{$IFDEF WINDOWS}
  hProductInfo:= LoadLibrary (PChar('KERNEL32.DLL'));
  Pointer(GetProductInfo) := GetProcAddress(hProductInfo,  'GetProductInfo');
  Pointer(GetVersionEx):= GetProcAddress(hProductInfo, 'GetVersionExA');
{$ENDIF}

finalization
  {$IFDEF WINDOWS}
  try
    if hProductInfo<>0 then  FreeLibrary(hProductInfo);

  except
  end;
{$ENDIF}

end.

