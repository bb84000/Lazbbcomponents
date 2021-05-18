//******************************************************************************
// Easter Component unit
// bb - sdtp May 2021
// Provides Easter dates and associated
// Parameters:
//   EasterYear (Integer): Selected year for Easter dates
// Properties :
//   InvalidYear (Boolean) True if year is invalid
//   Easterdate (DateTime) : Easter date
//   EasterMondaydate (TDateTime) : Easter monday date
//   Ascensdate (TDateTime) : Ascension date
//   Pentecdate (TDateTime) : Pentecost date
//   PentecMondaydate (TDateTime) : Pentecost monday date
//******************************************************************************

unit Easter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TEaster = class(TComponent)
  private
    fVersion: String;
    fEasteryear: Integer;
    fEasterdate: TDateTime;
    fEasterMondaydate: TDateTime;
    fAscensdate: TDatetime;
    fPentecdate: Tdatetime;
    fPentecMondaydate: Tdatetime;
    fInvalidYear: Boolean;
    procedure GetEaster;
    procedure setEasteryear(value: Integer);
  protected

  public
    constructor create(AOwner: TComponent); override;
    property InvalidYear: Boolean read fInvalidYear;
    property Easterdate : TDateTime read fEasterdate;
    property EasterMondaydate : TDateTime read fEasterMondaydate;
    property Ascensdate: TDateTime read fAscensdate;
    property Pentecdate: TDateTime read fPentecdate;
    property PentecMondaydate: TDateTime read fPentecMondaydate;
  published
    property Version: String read fVersion;
    property EasterYear: Integer read fEasteryear write setEasteryear;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I easter_icon.lrs}
  RegisterComponents('lazbbAstroComponents',[TEaster]);
end;

constructor TEaster.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fVersion:= '1.0';
  fEasteryear:= CurrentYear;
end;

procedure TEaster.setEasteryear(value: Integer);
begin
  if fEasterYear<>value then
  begin
    fEasterYear:= value;
    if not (csDesigning in ComponentState) then GetEaster;
  end;
end;

procedure TEaster.GetEaster;     // Wikipedia
var
  nMonth, nDay, nMoon, nEpact, nSunday, nGold, nCent, nCorx, nCorz: Integer;
begin
  fInvalidYear:= false;
  nGold := (fEasteryear mod 19) + 1;    // The Golden Number of the year in the 19 year Metonic Cycle
  nCent := (fEasteryear div 100) + 1;   // Calculate the Century
  { Number of years in which leap year was dropped in order... }
  { to keep in step with the sun: }
  nCorx := (3 * nCent) div 4 - 12;
  nCorz := (8 * nCent + 5) div 25 - 5; // Special correction to syncronize Easter with moon's orbit
  nSunday := (Longint(5) * fEasteryear) div 4 - nCorx - 10;  // Find Sunday
  { ^ To prevent overflow at year 6554}
  { Set Epact - specifies occurrence of full moon: }
  nEpact := (11 * nGold + 20 + nCorz - nCorx) mod 30;
  if nEpact < 0 then
    nEpact := nEpact + 30;
  if ((nEpact = 25) and (nGold > 11)) or (nEpact = 24) then nEpact := nEpact + 1;
  { Find Full Moon: }
  nMoon := 44 - nEpact;
  if nMoon < 21 then
    nMoon := nMoon + 30;
  { Advance to Sunday: }
  nMoon := nMoon + 7 - ((nSunday + nMoon) mod 7);
  if nMoon > 31 then
  begin
    nMonth := 4;
    nDay   := nMoon - 31;
  end
  else
  begin
    nMonth := 3;
    nDay   := nMoon;
  end;
  try
    fEasterdate := EncodeDate(fEasteryear, nMonth, nDay);
    fEasterMondaydate:= feasterdate+1;
    fAscensdate:= fEasterdate+39;
    fPentecdate:= fEasterdate+49;
    fPentecMondaydate:= fEasterdate+50;
  except
    fInvalidYear:= true;
  end;
end;

end.
