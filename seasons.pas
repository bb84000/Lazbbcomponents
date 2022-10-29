unit Seasons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, math;

type
  TSeasons = class(TComponent)
  private
    fSeasyear: Integer;
    fTimeZone: Double;
    fSpringdate: TDateTime;
    fSummerDate: TdateTime;
    fAutumnDate: TdateTime;
    fWinterDate: TDateTime;
    procedure setSeasyear(value: Integer);
    procedure SetTimeZone(Value: Double);
    function GetSeasonDate(num: Integer): TDateTime;
    function Periodic24(t: Double): Double;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    property SpringDate: TDateTime read fSpringdate;
    property SummerDate: TdateTime read fSummerDate;
    property AutumnDate: TdateTime read fAutumnDate;
    property WinterDate: TDateTime read fWinterDate;
  published
    property Seasyear: Integer read fSeasyear write setSeasyear;
    property Timezone: Double read fTimezone write setTimezone;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I seasons_icon.lrs}
  RegisterComponents('lazbbAstroComponents',[TSeasons]);
end;


constructor TSeasons.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSeasyear:= CurrentYear;
  fTimeZone:= 0;
end;

procedure TSeasons.setSeasyear(value: Integer);
begin
  if fSeasyear<>value then
  begin
    fSeasyear:= value;
    if not (csDesigning in ComponentState) then
    begin
      fSpringdate:= GetSeasonDate(0);
      fSummerdate:= GetSeasonDate(1);
      fAutumnDate:= GetSeasonDate(2);
      fWinterDate:= GetSeasonDate(3);
    end;
  end;
end;

procedure TSeasons.SetTimeZone(Value: Double);
begin
  if (fTimeZone <> Value) and (Value >= -12) and (Value <= +12) then
  begin
    fTimeZone := Value;
    if not (csDesigning in ComponentState) then
    begin
      fSpringdate:= GetSeasonDate(0);
      fSummerdate:= GetSeasonDate(1);
      fAutumnDate:= GetSeasonDate(2);
      fWinterDate:= GetSeasonDate(3);
    end;
  end;
end;

// Meeus Astronmical Algorithms Chapter 27

function TSeasons.GetSeasonDate(num: Integer): TDateTime;
var
  dDate: TDateTime;
  jdeo, yr, t, w, dl, s, julDay: Double;
  deltaT: Double;
  scl: Double;
begin
  // Caclul initial du jour julien
  yr:=(fSeasyear-2000)/1000;
  Case num of
    0: jdeo:= 2451623.80984 + 365242.37404*yr + 0.05169*power(yr,2) - 0.00411*power(yr,3) - 0.00057*power(yr,4);
    1: jdeo:= 2451716.56767 + 365241.62603*yr + 0.00325*power(yr,2) + 0.00888*power(yr,3) - 0.00030*power(yr,4);
    2: jdeo:= 2451810.21715 + 365242.01767*yr - 0.11575*power(yr,2) + 0.00337*power(yr,3) + 0.00078*power(yr,4);
    3: jdeo:= 2451900.05952 + 365242.74049*yr - 0.06223*power(yr,2) - 0.00823*power(yr,3) + 0.00032*power(yr,4);
  else
    jdeo:= 0;
  end;
  t:= (jdeo - 2451545.0)/36525;
  w:= (35999.373*t) - 2.47;
  dl:= 1 + 0.0334*cos(DegToRad(w))  + 0.0007*cos(DegToRad(2*w));
    // Correction périodique
    s:= Periodic24(t);
    julDay:= jdeo + ( (0.00001*s) / dL ); 	// This is the answer in Julian Emphemeris Days
    // écart entre UTC et DTD en secondes entre les années from Meeus Astronmical Algroithms Chapter 10
    scl:= (fSeasyear - 2000) / 100;
    deltaT:= 102 + 102*scl + 25.3*power(scl,2);
    // Special correction to avoid discontinurity in 2000
    if (fSeasyear >=2000) and (fSeasyear <=2100) then deltaT:= deltaT+ 0.37 * (fSeasyear - 2100 );
    // Ecart en jour fractionnaire
    deltaT:= deltaT/86400;
    // On y est ! Conversion en date réelle
    dDate:= julDay-deltaT-693594-1721425+0.5;  //DateDelta= 693594 + 1721425-0,5;
   Result:= dDate+fTimeZone/24;
end;

function TSeasons.Periodic24(t: Double ): Double;
const
  A: array[0..23] of integer = (485,203,199,182,156,136,77,74,70,58,52,50,45,44,29,18,17,16,14,12,12,12,9,8);
  B: array[0..23] of real = (324.96,337.23,342.08,27.85,73.14,171.52,222.54,296.72,243.58,119.81,297.17,21.02,
		     247.54,325.15,60.93,155.12,288.79,198.04,199.76,95.39,287.11,320.81,227.73,15.45);
  C: array[0..23] of real = (1934.136,32964.467,20.186,445267.112,45036.886,22518.443,
			     65928.934,3034.906,9037.513,33718.147,150.678,2281.226,
                             29929.562,31555.956,4443.417,67555.328,4562.452,62894.029,
			     31436.921,14577.848,31931.756,34777.259,1222.114,16859.074);
var
  i: Integer;
begin
  result:= 0;
  for i:= 0 to 23 do
    //result:= result +  A[i]*degCOS(;
    result:= result +  A[i]*cos(DegToRad(B[i] + (C[i]*T)));
end;

end.
