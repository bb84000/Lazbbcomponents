{******************************************************************************
 lazbbosversion - Returns OS version information (Windows, Linux and Mac
 Component version, replace previous units
 sdtp - bb - june 2024
 Some windows functions and windows structures are dynamically loaded in
   lazbbosversiobnabse unit
 Localization data in application .lng file
 - 29 June 2024 : Replaced windows version strings constants with resources
                  and/or csv files (to be placed in lang folder)
 - 24 september 2024 : updated for windows 11 24H2 build
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
  Classes, SysUtils, LResources, lazbbosversionbase, lazbbinifiles, dialogs;

type
  TbbOsVersion = class(TComponent)
  private
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
    Init: Boolean;
   {$IFDEF WINDOWS}
      fProdStrs: TStrings;
      Win10Strs: TStrings;
      Win11Strs: Tstrings;
      procedure SetProdStrs(const value: TStrings);
      procedure ListChanged(Sender: Tobject);
      function IsWin64: Boolean;
      procedure GetNT32Info;
    {$ENDIF}
  protected

  public
    constructor Create(aOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure GetSysInfo;
    procedure Translate(LngFile: TBbIniFile);
  published
    {$IFDEF WINDOWS}
    property VerMaj: integer read FVerMaj;      // major version number
    property VerMin: integer read FVerMin;      // Minor version number
    property VerBuild: integer read FVerBuild;  // Build number
    property VerSup : String read FVerSup;      // Additional version information
    property VerMask : Integer read FVerMask;   // Product suite mask;
    property VerTyp: integer read FVerTyp;      // Windows type
    property VerProd : String read FVerProd;    // Version type
    property ProdStrs: TStrings read fProdStrs write SetProdStrs;
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
    ProductStrs= ''+LineEnding+
                 'Home'+LineEnding+
                 'Professional'+LineEnding+
                 'Server';

    // First element: build number, second element: english
    // Replaced with resources
    {Windows10Strs = '00000=Unknown version'+LineEnding+
                    '10240=v 1507 "July 2015 update"'+LineEnding+
                    '10586=v 1511 "November 2015 update"'+LineEnding+
                    '14393=v 1607 "July 2016 (Anniversary update)"'+LineEnding+
                    '15063=v 1703 "April 2017 (Creators update)"'+LineEnding+
                    '16299=v 1709 "October 2017 (Fall Creators update)"'+LineEnding+
                    '17134=v 1803 "April 2018 update"'+LineEnding+
                    '17763=v 1809 "October 2018 update"'+LineEnding+
                    '18362=v 1903 "May 2019 update"'+LineEnding+
                    '18363=v 1909 "November 2019 update"'+LineEnding+
                    '19041=v 2004 "May 2020 update"'+LineEnding+
                    '19042=v 20H2 "October 2020 update"'+LineEnding+
                    '19043=v 21H1 "May 2021 update"'+LineEnding+
                    '19044=v 21H2 "November 2021 update"'+LineEnding+
                    '19045=v 22H2 "October 2022 update"';


    Windows11Strs = '00000=Unknown version'+LineEnding+
                    '22000=v 21H2 "October 2021 Initial version"'+LineEnding+
                    '22621=v 22H2 "September 2022 update"'+LineEnding+
                    '22631=v 23H2 "October 2023 update"'+
                    '26100=v 24H2 "October 2024 update"'; }


var
    fVerProEx: DWORD;

 {$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  {$I lazbbosversion_icon.lrs}
  {$R lazbbosversion_defs.rc}     //OS versions builds and names
  RegisterComponents('lazbbcomponents',[TbbOsVersion]);
end;

constructor TbbOsVersion.Create(aOwner: Tcomponent);
{$IFDEF WINDOWS}
const
  RT_RCDATA = MAKEINTRESOURCE(10);
  // nested function
  function PopulateWinList(sl: TStrings; resname: String): Boolean;
  var
    rs: TStream;
    begin
      result:= false;
      TStringList(sl).OnChange:= @ListChanged;
      // Versions files can be located in lang subfolder
      // File content not tested. Be prudent !

      if FileExists('lang\'+resname+'.csv') then
      try
        rs:= TFileStream.Create('lang\'+resname, fmOpenRead);
        result:= true;
      except
      end;
      if result=false then     //file not found or read error, load resource
      try
        rs := TResourceStream.Create(HINSTANCE, UpperCase(resname), RT_RCDATA);
      except
        rs := TStringStream.Create('00000;Unknown version');
      end;
      // Now transfer stream to string list
      try
        sl.LoadFromStream(rs);
      finally
        result:= true;
        rs.Free;
      end;
  end;  // function end
{$ENDIF}
begin
  inherited Create(aOwner);
  // Initialize variables
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
     // Create and populate product list
     fProdStrs:= TstringList.Create;
     TStringList(fProdStrs).OnChange:= @ListChanged;
     fProdStrs.Text:= ProductStrs;
     // Create and populate Windows 10 version list property
     Win10Strs:= TstringList.Create;
     PopulateWinList(Win10Strs, 'windows10_en');
     // Windows 11
     Win11Strs:= TstringList.Create;
     PopulateWinList(Win11Strs, 'windows11_en');
  {$ENDIF}
  init:= true;
  GetSysInfo;
end;

destructor TbbOsVersion.Destroy;
begin
{$IFDEF WINDOWS}
  if assigned(fProdStrs) then fProdStrs.free;
  if assigned(Win10Strs) then Win10Strs.free;
  if assigned(Win11Strs) then Win11Strs.free;
{$ENDIF}
  inherited;
end;

{$IFDEF WINDOWS}

procedure TbbOsVersion.SetProdStrs(const value: TStrings);
begin
  if fProdStrs<>value then
  begin
    fProdStrs.Assign(value);
  end;
end;

procedure TbbOsVersion.ListChanged(Sender: Tobject);
begin
  // Be sure all is intialized
  if init then GetSysInfo;
end;

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
    fVerProd:= ProdStrs.Strings[fVerPro];//ProdStr[fVerPro];
  except
    fVerProd:= ProdStrs.Strings[0];//ProdStr[0];
  end;
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
  A: TStringArray;
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
                    A:= Win10Strs.Strings[0].Split(';'); //'Unknown version'
                    FVersup:= A[1];
                    for i:= 0 to Win10Strs.Count-1 do
                    begin
                      A:= Win10Strs.Strings[i].Split(';');
                      if FVerBuild=StrToInt(A[0]) then
                      begin
                        FVersup:= A[1];
                        break;
                      end;
                    end;
                  end else
                  begin
                    fVerTyp:= 22  ;  // Windows 11 build number start with 22000
                    A:= Win11Strs.Strings[0].Split(';'); //'Unknown version'
                    FVersup:= A[1];
                    for i:= 0 to Win11Strs.Count-1 do
                    begin
                      A:= Win11Strs.Strings[i].Split(';');
                      if FVerBuild=StrToInt(A[0]) then
                      begin
                        FVersup:= A[1];
                        break;
                      end;
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
  procedure TbbOSVersion.GetSysInfo;
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

procedure TbbOSVersion.Translate(LngFile: TBbIniFile);
var
  i: Integer;
  A: TStringArray;
begin
  if assigned (Lngfile) then
  with LngFile do
  begin
    {$IFDEF WINDOWS}
    ProdStrs.Strings[1]:= ReadString('OSVersion','Home','Famille'); ;
    ProdStrs.Strings[2]:= ReadString('OSVersion','Professional','Entreprise');
    ProdStrs.Strings[3]:= ReadString('OSVersion','Server','Serveur');
    for i:= 0 to Win10Strs.count-1 do
    begin
      A:= Win10Strs.Strings[i].split(';');
      Win10Strs.Strings[i]:= A[0]+';'+ReadString('OSVersion',A[0],A[1]);
    end;
    for i:= 0 to Win11Strs.count-1 do
    begin
      A:= Win11Strs.Strings[i].split(';');
      Win11Strs.Strings[i]:= A[0]+';'+ReadString('OSVersion',A[0],A[1]);
    end;
    {$ENDIF}
  end;
end;

end.
