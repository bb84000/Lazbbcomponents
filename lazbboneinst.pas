//*******************************************************************************
//   bbOneInst is a component to avoid open multiple instances of an application
//   It is derived from UniqueInstance component  (Copyright (C) 2006 Luiz Americo Pereira Camara
//   pascalive@bol.com.br)
//
//   bbOneInst can differentiate instances of an application using 1st parameter. Set
//   UseParameter to true to enable this differentiation

// bb - sdtp - september 2022
//*******************************************************************************

unit lazbbOneInst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, simpleipc, ExtCtrls, LazUTF8, StrUtils;

type
  TOnOtherInstance = procedure (Sender : TObject; Parameter: String) of object;

  TbbOneInst = class(TComponent)
  private
    FIdentifier: String;
    FUseParameter: Boolean;
    FOnOtherInstance: TOnOtherInstance;
    FUpdateInterval: Cardinal;
    FEnabled: Boolean;
    FPriorInstanceRunning: Boolean;
    IPCServer: TSimpleIPCServer;
    IPCClient: TSimpleIPCClient;
    {$ifdef mswindows}
      procedure MessageQueued(Sender: TObject);
    {$else} {unix}
    procedure receivemessage(Sender: TObject); {receive paramstr(1) from second instance prior to termination}
    {$endif}

  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    property PriorInstanceRunning: Boolean read FPriorInstanceRunning;
  published
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Identifier: String read FIdentifier write FIdentifier;
    property UseParameter: Boolean read FUseParameter write FUseParameter default False;
    property UpdateInterval: Cardinal read FUpdateInterval write FUpdateInterval default 1000;
    property OnOtherInstance: TOnOtherInstance read FOnOtherInstance write FOnOtherInstance;
  end;

const
    BaseId = 'OneInst_';

procedure Register;

implementation

procedure Register;
begin
  {$I lazbboneinst_icon.lrs}
  RegisterComponents('lazbbComponents',[TbbOneInst]);
end;

constructor TbbOneInst.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdateInterval := 1000;
end;



{$ifdef mswindows}
procedure TbbOneInst.MessageQueued(Sender: TObject);
{$else} {unix}
var
  Timer: TTimer;

procedure TbbOneInst.ReceiveMessage(Sender: TObject);
{$endif}
var
  Parameter: String;
begin
   if IPCServer.PeekMessage(1,True) then
   begin
     Parameter := IPCServer.StringMessage;
     FOnOtherInstance(Self, Parameter);
   end;
end;

procedure TbbOneInst.Loaded;
begin
  if not (csDesigning in ComponentState) and FEnabled then
  begin
    if FIdentifier='' then FIdentifier:= BaseId+ExtractFileName(ParamStrUTF8(0));
    if FUseParameter then FIdentifier:= FIdentifier+'_'+ParamStrUTF8(1);
    IPCClient := TSimpleIPCClient.Create(Self);
    IPCClient.ServerId := FIdentifier;
    if not Assigned(IPCServer) and IPCClient.ServerRunning then
    begin
      //A older instance is running.
      FPriorInstanceRunning := True;
      //A instance is already running
      //Send a message and then exit
      if Assigned(FOnOtherInstance) then
      begin
        IPCClient.Active := True;
        IPCClient.SendStringMessage(ParamStrUTF8(1));
      end;
      Application.ShowMainForm := False;
      Application.Terminate;
    end
    else
    begin
      if not Assigned(IPCServer) then
      begin
        IPCServer := TSimpleIPCServer.Create(nil);
        IPCServer.ServerID := FIdentifier;
        IPCServer.Global := True;
        IPCServer.StartServer;
      end;
      {$ifdef mswindows}
      IPCServer.OnMessageQueued:= @MessageQueued;
      {$else}
      Timer := TTimer.Create(nil); {In Linux no event occurs in MessageQueued. Trigger receivemessage by timer}
      Timer.Interval := FUpdateInterval;
      Timer.OnTimer := @ReceiveMessage;
      {$endif}
      //there's no more need for IPCClient
      IPCClient.Destroy;
    end;
  end;
  inherited;
end;




end.
