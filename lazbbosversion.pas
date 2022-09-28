{******************************************************************************
 lazbbosversion - Returns OS version information (Windows, Linux and Mac
 Component version, replace previous units
 sdtp - bb - september 2022
 Some windows functions and windows structures are dynamically loaded in
   lazbbosversiobnabse unit
 Localization data in application .lng file
******************************************************************************}

unit lazbbOsVersion;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS}
    Windows,
  {$ELSE}
    process,
  {$ENDIF}
  Classes, SysUtils, LResources, lazbbutils, Forms, Controls, Graphics, Dialogs,
  lazbbosversionbase;

type
  TbbOsVersion = class(TComponent)
  private
    //fProdStr: TStrings ;
    FPID : Integer; {platform ID}
    FVerMaj, FVerMin, FVerBuild: Integer;
    FVerSup : String;
    FSrvPMaj, fSrvPMin: Word;
    FVerMask : Integer;
    fProdTyp, fReserved: BYTE;
    FVerTyp, FVerPro : Integer;
    FVerProd: String;
    fOSName: string;
    fArchitecture: string;
    FVerDetail: string;     //Description of the OS, with version, build etc.
    // Unix
    fKernelName: string;
    fKernelRelease: string;
    fKernelVersion: string;
    fNetworkNode: string;
   {$IFDEF WINDOWS}
      function IsWin64: Boolean;
      procedure GetNT32Info;
    {$ENDIF}
  protected

  public
    ProdStr: array of String;
    Win10Build: array of array of String;
    Win11Build: array of array of String;
    constructor Create(aOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure GetSysInfo;
  published
    {$IFDEF WINDOWS}
    property VerMaj: integer read FVerMaj;      // major version number
    property VerMin: integer read FVerMin;      // Minor version number
    property VerBuild: integer read FVerBuild;  // Build number
    property VerSup : String read FVerSup;      // Additional version information
    property VerMask : Integer read FVerMask;   // Product suite mask;
    property VerTyp: integer read FVerTyp;      // Windows type
    property VerProd : String read FVerProd;    // Version type
    {$ELSE}
    property KernelName: string read FKernelName;
    property KernelRelease: string read FKernelRelease;
    property KernelVersion: string read FKernelVersion;
    property NetworkNode: string read FNetworkNode;
    {$ENDIF}
    property OSName: string read FOSName;
    property Architecture: string read fArchitecture;
    property VerDetail: string read FVerDetail; //Description of the OS, with version, build etc.


 end;

{$IFDEF WINDOWS}
  const
    // Valeurs en hexa pour info
    ProdStrEx: array [0..$A3] of String =('Unknown product',                                     //00
                                         'Ultimate Edition',                                    //01
                                         'Home Basic Edition',                                  //02
                                         'Home Premium Edition',                                //03
                                         'Enterprise',                                          //04
                                         'Home Basic Edition',                                  //05
                                         'Business',                                            //06
                                         'Server Standard Edition (full installation)',         //07
                                         'Server Datacenter (full installation)',               //08
                                         'Small Business Server',                               //09
                                         'Server Enterprise Edition (full installation)',       //0A
                                         'Starter Edition',                                     //0B
                                         'Server Datacenter (core installation)',               //0C
                                         'Server Standard Edition (core installation)',         //0D
                                         'Server Enterprise Edition (core installation)',       //0E
                                         'Server Enterprise Edition for Itanium-based Systems', //0F
                                         'Business N',                                          //10
                                         'Web Server Edition',                                  //11
                                         'Cluster Server',                                      //12
                                         'Home Server Edition',                                 //13
                                         'Storage Server Express Edition',                      //14
                                         'Storage Server Standard Edition',                     //15
                                         'Storage Server Workgroup Edition',                    //16
                                         'Storage Server Enterprise Edition',                   //17
                                         'Server for Small Business Edition',                   //18
                                         'Small Business Server Premium Edition',               //19
                                         'Home Premium Edition',                                //1A
                                         'Enterprise N',                                        //1B
                                         'Ultimate Edition',                                    //1C
                                         'Web Server (core installation)',                      //1D
                                         'Windows Essential Business Server Management Server', //1E
                                         'Windows Essential Business Server Security Server',   //1F
                                         'Windows Essential Business Server Messaging Server',  //20
                                         'Server Foundation',                                   //21
                                         'Windows Home Server 2011',                            //22
                                         'Windows Server 2008 without Hyper-V for Windows Essential Server Solutions',   //23
                                         'Server Standard without Hyper-V',                       //24
                                         'Server Datacenter without Hyper-V (full installation)', //25
                                         'Server Enterprise without Hyper-V (full installation)', //26
                                         'Server Datacenter without Hyper-V (core installation)', //27
                                         'Server Standard without Hyper-V (core installation)',   //28
                                         'Server Enterprise without Hyper-V (core installation)', //29
                                         'Microsoft Hyper-V Server',                              //2A
                                         'Storage Server Express (core installation)',            //2B
                                         'Storage Server Standard (core installation)',           //2C
                                         'Storage Server Workgroup (core installation)',          //2D
                                         'Storage Server Enterprise (core installation)',         //2E
                                         'Starter N',                                             //2F
                                         'Professional',                                          //30
                                         'Professional N',                                        //31
                                         'Windows Small Business Server 2011 Essentials',         //32
                                         'Server For SB Solutions',                               //33
                                         'Server Solutions Premium',                              //34
                                         'Server Solutions Premium (core installation)',          //35
                                         'Server For SB Solutions EM',                            //36
                                         'Server For SB Solutions EM',                            //37
                                         'Windows MultiPoint Server',                             //38
                                         'Unknown',                                               //39
                                         'Unknown',                                               //3A
                                         'Windows Essential Server Solution Management',          //3B
                                         'Windows Essential Server Solution Additional',          //3C
                                         'Windows Essential Server Solution Management SVC',      //3D
                                         'Windows Essential Server Solution Additional SVC',      //3E
                                         'Small Business Server Premium (core installation)',     //3F
                                         'Server Hyper Core V',                                   //40
                                         'Unknown',                                               //41
                                         'Not supported',                                         //42
                                         'Not supported',                                         //43
                                         'Not supported',                                         //44
                                         'Not supported',                                         //45
                                         'Enterprise E',                                          //46
                                         'Not supported',                                         //47
                                         'Enterprise (evaluation)',                               //48
                                         'Unknown',                                               //49
                                         'Unknown',                                               //4A
                                         'Unknown',                                               //4B
                                         'Windows MultiPoint Server Standard (full)',             //4C
                                         'Windows MultiPoint Server Premium (full)',              //4D
                                         'Unknown',                                               //4E
                                         'Server Standard (evaluation)',                          //4F
                                         'Server Datacenter (evaluation)',                        //50
                                         'Unknown',                                               //51
                                         'Unknown',                                               //52
                                         'Unknown',                                               //53
                                         'Enterprise N (evaluation)',                             //54
                                         'Unknown',                                               //55
                                         'Unknown',                                               //56
                                         'Unknown',                                               //57
                                         'Unknown',                                               //58
                                         'Unknown',                                               //59
                                         'Unknown',                                               //5A
                                         'Unknown',                                               //5B
                                         'Unknown',                                               //5C
                                         'Unknown',                                               //5D
                                         'Unknown',                                               //5E
                                         'Storage Server Workgroup (evaluation)',                 //5F
                                         'Storage Server Standard (evaluation)',                  //60
                                         'Unknown',                                               //61
                                         'Home N',                                                //62
                                         'Home China',                                            //63
                                         'Home Single Language',                                  //64
                                         'Home',                                                  //65
                                         'Unknown',                                               //66
                                         'Professional with Media Center',                        //67
                                         'Unlicensed product',                                    //68
                                          'Unknown',                                              //69
                                         'Unknown',                                               //6A
                                         'Unknown',                                               //6B
                                         'Unknown',                                               //6C
                                         'Unknown',                                               //6D
                                         'Unknown',                                               //6E
                                         'Unknown',                                               //6F
                                         'Unknown',                                               //70
                                         'Unknown',                                               //71
                                         'Unknown',                                               //72
                                         'Unknown',                                               //73
                                         'Unknown',                                               //74
                                         'Unknown',                                               //75
                                         'Unknown',                                               //76
                                         'Unknown',                                               //77
                                         'Unknown',                                               //78
                                         'Education',                                             //79
                                         'Education N',                                           //7A
                                         'Unknown',                                               //7B
                                         'Unknown',                                               //7C
                                         'Enterprise 2015 LTSB',                                  //7D
                                         'Enterprise 2015 LTSB N',                                //7E
                                         'Unknown',                                               //7F
                                         'Unknown',                                               //80
                                         'Enterprise 2015 LTSB (evaluation)',                     //81
                                          'Unknown',                                              //82
                                         'Unknown',                                               //83
                                         'Unknown',                                               //84
                                         'Unknown',                                               //85
                                         'Unknown',                                               //86
                                         'Unknown',                                               //87
                                         'Unknown',                                               //88
                                         'Unknown',                                               //89
                                         'Unknown',                                               //8A
                                         'Unknown',                                               //8B
                                         'Unknown',                                               //8C
                                         'Unknown',                                               //8D
                                         'Unknown',                                               //8E
                                         'Unknown',                                               //8F
                                         'Unknown',                                               //90
                                         'Server Datacenter, Semi-Annual Channel (core)',         //91
                                         'Server Standard, Semi-Annual Channel (core)',           //92
                                         'Unknown',                                               //93
                                         'Unknown',                                               //94
                                         'Unknown',                                               //95
                                         'Unknown',                                               //96
                                         'Unknown',                                               //97
                                         'Unknown',                                               //98
                                         'Unknown',                                               //99
                                         'Unknown',                                               //9A
                                         'Unknown',                                               //9B
                                         'Unknown',                                               //9C
                                         'Unknown',                                               //9D
                                         'Unknown',                                               //9E
                                         'Unknown',                                               //9F
                                         'Unknown',                                               //A0
                                         'Pro for Workstations',                                  //A1
                                         'Windows 10 Pro for Workstations',                       //A2
                                         'Unknown');                                              //A3

    ProductStr: array of String =   ('',
                                         'Home',
                                         'Professional',
                                         'Server');

    StatStr: array of String = ('Microsoft Windows 32',
                                'Microsoft Windows 95',
                                'Microsoft Windows 95-OSR2',
                                'Microsoft Windows 98',
                                'Microsoft Windows 98 SE',
                                'Microsoft Windows ME',
                                'Microsoft Windows NT 3.5',
                                'Microsoft Windows NT 4',
                                'Microsoft Windows 2000',
                                'Microsoft Windows XP',
                                'Microsoft Windows Server 2003',
                                'Microsoft Windows Vista',
                                'Microsoft Windows Server 2008',
                                'Microsoft Windows Server 2008 R2',
                                'Microsoft Windows 7',
                                'Microsoft Windows 8',
                                'Microsoft Windows Server 2012',
                                'Microsoft Windows 8.1',
                                'Windows Server 2012 R2',
                                'Microsoft Windows 10',
                                'Windows Server 2016',
                                'Windows Server 2019',
                                'Microsoft Windows 11',
                                'Windows Server 2022',
                                'Système inconnu');
        // First element: build number, second element: english

    Windows10Build: array of array [0..1] of String =(('00000',    'Unknown version'),
                                              ('10240', 'v 1507 "July 2015 update"'),
                                              ('10586', 'v 1511 "November 2015 update"'),
                                              ('14393', 'v 1607 "July 2016 (Anniversary update)"'),
                                              ('15063', 'v 1703 "April 2017 (Creators update)"'),
                                              ('16299', 'v 1709 "October 2017 (Fall Creators update)"'),
                                              ('17134', 'v 1803 "April 2018 update"'),
                                              ('17763', 'v 1809 "October 2018 update"'),
                                              ('18362', 'v 1903 "May 2019 update"'),
                                              ('18363', 'v 1909 "November 2019 update"'),
                                              ('19041', 'v 2004 "May 2020 update"'),
                                              ('19042', 'v 20H2 "October 2020 update"'),
                                              ('19043', 'v 21H1 "May 2021 update"'),
                                              ('19044', 'v 21H2 "November 2021 update"'),
                                              ('19045', 'v 22H2 "October 2022 update"'));

    Windows11Build: array of array [0..1] of String = (('00000',    'Unknown version'),
                                              ('22000', 'v 21H2 "October 2021 Initial version"'),
                                              ('22621', 'v 22H2 "September 2022 update"'));



var
    fVerProEx: DWORD;

 {$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  {$I lazbbosversion_icon.lrs}
  RegisterComponents('lazbbcomponents',[TbbOsVersion]);
end;

constructor TbbOsVersion.Create(aOwner: Tcomponent);
var
  i: Integer;
begin
  inherited Create(aOwner);
  // Initialize variables
  //fProdStr:= TStringList.Create;
  FVerMaj:=0;
  FVerMin:=0;
  FVerBuild:=0;
  FVerSup:='';
  FVerMask:=0;
  FVerTyp:=0 ;
  FVerProd:='';
  FOSName:='';
  fArchitecture:='';
  FKernelName:='';
  FKernelRelease:='';
  FKernelVersion:='';
  FNetworkNode:='';
  FVerDetail:='';
  {$IFDEF WINDOWS}
     // populate dynamic arrays for product details and versions with default values
     SetLength(ProdStr, Length(ProductStr));
     for i:= 0 to high(ProdStr) do ProdStr[i]:= ProductStr[i];
     SetLength(Win10Build, length(Windows10Build), length(Windows10Build[0]));
     SetLength(Win11Build, length(Windows11Build), length(Windows11Build[0]));
     // Windows 10
     for i:= 0 to high(Win10Build) do Win10Build[i,0]:= Windows10Build[i,0];
     for i:= 0 to high(Win10Build) do Win10Build[i,1]:= Windows10Build[i,1];
     // Windows 11
     for i:= 0 to high(Win11Build) do Win11Build[i,0]:= Windows11Build[i,0];
     for i:= 0 to high(Win11Build) do Win11Build[i,1]:= Windows11Build[i,1];
  {$ENDIF}
  GetSysInfo;
end;

destructor TbbOSVersion.Destroy;
begin
  {$IFDEF WINDOWS}
   try
     //if hProductInfo<>0 then  FreeLibrary(hProductInfo);
   except
   end;
 {$ENDIF}
  inherited;
end;

{$IFDEF WINDOWS}
function TbbOsVersion.IsWin64: Boolean;
  {$IFDEF WIN32}
  type
    TIsWow64Process = function(Handle: Windows.THandle; var Res: Windows.BOOL): Windows.BOOL; stdcall;
  var
    IsWOW64: Windows.BOOL;
    IsWOW64Process: TIsWow64Process;
  {$ENDIF}
begin
  {$IFDEF WIN32}
  // Try to load required function from kernel32
  IsWOW64Process := TIsWow64Process(Windows.GetProcAddress(Windows.GetModuleHandle('kernel32'), 'IsWow64Process'));
  if Assigned(IsWOW64Process) then
    begin
      // Function exists
      if not IsWOW64Process(Windows.GetCurrentProcess, IsWOW64) then
        Result:=False
      else
        Result:=IsWOW64;
    end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
  {$ELSE} //if were running 64bit code, OS must be 64bit !)
     Result := True;
  {$ENDIF}
end;

procedure TbbOSVersion.GetSysInfo;
var
  OsViEx : TOSVersionInfoEx;
begin
  fVerProEx:= 0;
  // Free Pascal GetVersionEx function use OSVersionInfo structure instead OSVersionInfoEx,
  // So, we have redefined it
  OsViEx:= Default(TOSVersionInfoEx);
  OsViEx.dwOSVersionInfoSize:= SizeOf(TOSVersionInfoEx);
  // Before W2000 this function doesn't exists; so we exit
  if not (assigned(GetVersionEx) and GetVersionEx (OsViEx)) then exit;
  With OsViEx do
  begin
    fVerMaj:=dwMajorVersion;
    fVerMin:=dwMinorVersion;
    fVerBuild:= dwBuildNumber and $FFFF;
    fVerSup:= StrPas(szCSDVersion);
    fPid:= dWPlatformID;
    fSrvPMaj:= wServicePackMajor;
    fSrvPMin:= wServicePackMinor;
    fVerMask:= wSuiteMask;
    fProdTyp:= wProductType;
    fReserved:= wReserved;
    // Inconnu par défaut
    fVerTyp:= High(StatStr);
    Case fPid of
    0 : fVerTyp:= 0;                                                       // Win32s
    1 : If fVerMin < 10 then
        begin
          If fVerBuild <= 1000 then fVerTyp:= 1                            // win95 4.00 build 950
          else fVerTyp:= 2;                                                // Win95-OSR2 4.00 950c
        end else
        begin
          if (fVerBuild >= 0) and (fVerBuild < 2000) then fVerTyp:= 3;     // Win98 4.10 build 1999
          if (fVerBuild >= 2000) and (fVerBuild < 3000) then fVerTyp:= 4;  // Win98 SE 4.10 build 2222
          if fVerBuild >= 3000 then fVerTyp:= 5 ;                          //Win ME 4.90 build 3000
        end;
    2: begin                                                               //VER_PLATFORM_WIN32_NT
         GetNT32Info;
       end;
    end;

  end;
  FOSName:= StatStr[High(StatStr)];
  if (fVerTyp < High(StatStr)) then FOSName:= StatStr[fVerTyp];
  try
    if fVerProEx > 0 then fVerProd:=  ProdStrEx[fVerProEx] else
    fVerProd:= ProdStr[fVerPro];
  except
    fVerProd:= ProdStr[0];
  end;
  //s:= GetEnvironmentVariable('PROCESSOR_ARCHITECTURE');
  //if s='AMD64' then s:= 'x86_64';
  if IsWin64 then
  fArchitecture:= 'x86_64' else
  fArchitecture:= 'x86';
  fVerDetail:= fOSName+' '+fVerProd+' - '+IntToStr(fVerMaj)+'.'+IntToStr(fVerMin)
                 +'.'+IntToStr(fVerBuild)+' - '+fVerSup+' - '+fArchitecture;
end;

procedure TbbOSVersion.GetNT32Info ;
var
  dwOSMajorVersion, dwOSMinorVersion,
  dwSpMajorVersion, dwSpMinorVersion: DWORD;
  i: integer;
begin
  dwOSMajorVersion:= 0;
  dwOSMinorVersion:= 0;
  dwSpMajorVersion:= 0;
  dwSpMinorVersion:= 0;
  case fVerMaj of
    3: fVerTyp:= 6;  //NT 3.5
    4: fVerTyp:= 7;  //NT 4
    5: case fVerMin of
         0: begin
              fVerTyp:= 8; // W2000
              if fProdTyp=VER_NT_WORKSTATION then fVerPro:= 2  // Professional
              else fVerPro:= 3;                                // Server
            end;
         1: begin
              fVerTyp:= 9; // Windows XP
              if (fVerMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL then fVerPro:= 1     //Home Edition
              else fVerPro:= 2;     //Professional
            end;
         2: fVerTyp:= 10; // Windows Server 2003
       end;
    6: begin
         if Assigned(GetProductInfo) then
         begin
           GetProductInfo( dwOSMajorVersion, dwOSMinorVersion,
                           dwSpMajorVersion, dwSpMinorVersion, fVerProEx );
           if fVerProEx = $ABCDABCD then fVerProEx:= High(ProdStrEx);
         end;
         case fVerMin of
           0: if fProdTyp= VER_NT_WORKSTATION then  // Windows Vista
              begin
                fVerTyp:= 11;                                                            // Windows Vista
                if (fVerMask and VER_SUITE_PERSONAL)=VER_SUITE_PERSONAL then fVerPro:= 1 //Home Edition
                else fVerPro:= 2;                                                        //Professional
              end else  fVerTyp:= 12;                                                    // Windows Server 2008
           1: if fProdTyp=VER_NT_WORKSTATION then
              begin
                fVerTyp:= 14;                                                            // Windows 7
                if (fVerMask and VER_SUITE_PERSONAL)=VER_SUITE_PERSONAL then fVerPro:= 1 //Home Edition
                else fVerPro:= 2;                                                        //Professional
              end else fVerTyp:= 13;                                                     // Windows Server 2008 RC2
           2: if fProdTyp =VER_NT_WORKSTATION then
              begin
                fVerTyp:= 15;                                                            // Windows 8
                if (fVerMask and VER_SUITE_PERSONAL)=VER_SUITE_PERSONAL then fVerPro:= 1 //Home Edition
                else fVerPro:= 2;                                                        //Professional
              end else fVerTyp:= 16;                                                     // Windows Server 2012
           3: if fProdTyp= VER_NT_WORKSTATION then
              begin
                fVerTyp:= 17;                                                            // Windows 8.1
                if (fVerMask and VER_SUITE_PERSONAL)=VER_SUITE_PERSONAL then fVerPro:= 1 //Home Edition
                else fVerPro:= 2;                                                        //Professional
              end else fVerTyp:= 18;                                                     // Windows 2012 Server R2
         end;     //case fVermin
       end;
    10: begin
          if Assigned(GetProductInfo) then
          begin
            GetProductInfo( dwOSMajorVersion, dwOSMinorVersion,
                             dwSpMajorVersion, dwSpMinorVersion, fVerProEx );
            if fVerProEx = $ABCDABCD then fVerProEx:= High(ProdStrEx);
          end;
          case fVerMin of                                     // Windows 10 , Windows 11
             0: if fProdTyp=VER_NT_WORKSTATION then
                begin
                  if (fVerMask and VER_SUITE_PERSONAL)=VER_SUITE_PERSONAL then fVerPro:= 1  //Home Edition
                  else fVerPro:= 2;                                                         //Professional
                  if FVerBuild < 22000 then
                  begin
                    fVerTyp:= 19;      // Windows 10 build number start with 10000
                    // Match builds to Win 10 version commercial name, Build numbers are in Win10build array
                    FVersup:= Win10Build[0, 1]; //'Unknown version';
                    for i:= 0 to high(Win10build) do
                      if FVerBuild=StringToInt(Win10build[i,0]) then
                      try
                        FVersup:= Win10Build[i, 1];
                        break;
                      except
                      end;
                  end else
                  begin
                    fVerTyp:= 22  ;  // Windows 11 build number start with 22000
                    FVersup:= Win11Build[0, 1]; //'Unknown version';
                    for i:= 0 to high(Win11build) do
                      if FVerBuild=StringToInt(Win11build[i,0]) then
                      try
                        FVersup:= Win11Build[i, 1];
                        break;
                      except
                      end;
                  end;
                end else
                begin
                  if fVerbuild < 14394 then
                  begin
                    fVerTyp:= 20;                           // Windows Server 2016
                  end else
                  begin
                     if fVerbuild < 20348 then fVerTyp:= 21 // Windows Server 2019
                     else fVerTyp:= 23;                     // Windows server 2022
                  end;
                end;
          end;// Case fVerMin
        end;  // Case 10
  end;        // Case fVermaj
end;

// End of Windows code, begin Linux, Unix or Mac code


{$ELSE}
  procedure TOSVersion.GetSysInfo;
  var
    P: TProcess;
    Function ExecParam(Param: String): String;
        Begin
          P.Parameters[0]:= '-' + Param;
          P.Execute;
          SetLength(Result, 1000);
          SetLength(Result, P.Output.Read(Result[1], Length(Result)));
          While (Length(Result) > 0) And (Result[Length(Result)] In [#8..#13,#32]) Do
            SetLength(Result, Length(Result) - 1);
        End;
    Begin
      //Default(OSInfo);
      P:= TProcess.Create(Nil);
      P.Options:= [poWaitOnExit, poUsePipes];
      P.Executable:= 'uname';
      P.Parameters.Add('');
      fOSName:= ExecParam('o');
      fKernelName:= ExecParam('s');
      fKernelRelease:= ExecParam('r');
      fKernelVersion:= ExecParam('v');
      fNetworkNode:= ExecParam('n');
      fArchitecture:= ExecParam('m');
      P.Free;
      fVerDetail:= fOSName+' '+fKernelName+' '+fKernelVersion;
    End;

{$ENDIF}



end.
