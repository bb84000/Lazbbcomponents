//******************************************************************************
// TSunTime v.2.0 -- Calculates times of sunrise, sunset.
//
// The algorithm is derived from noaa code :
// http://www.srrb.noaa.gov/highlights/sunrise/sunrise.html
// Adapted and ported on Free Pascal by bb - sdtp - may 2021
// Properties
//   Sundate (TDateTime) : Selected day
//   TimeZone (Double) : Time offset in hours
//   DST (Double) : Daylight saving time (0: no, 1: yes);
//   Latitude (Double) : Latitude in decimal degrees (N : positive, S : negative)
//   Longitude (Double) : Longitude in decimal degrees (E : positive, W : negative)
//   Altitude (Integer) : Not yet implemented
//   TypeSun (TSunrise) : Sunrise/sunset type: standard, civil, nautic ou astro
//
//   Sunrise (TDateTime) : Sunrise at the selected date (Sundate)
//   Sunset (TDateTime) : Sunset
//   SunNoon (TDateTime) : Noon
// Infos
//  ZenithDistance :  angular distance of a celestial body from the zenith.
//    90° minus the body's altitude above the horizon
//    Todo : Altitude/Elevation : adding -2.076*sqrt(elevation in metres)/60
//******************************************************************************

unit Suntime;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, math, dateUtils;

type
  // Type of value needed
  TSunrise = (standard, civil, nautic, astro, none);

  TSuntime = class(TComponent)
  private
    fVersion: String;
    fSundate: TDateTime;
    fTimeZone: Double;
    fDST: Double;
    fLatitude: Double;
    fLongitude: Double;
    fAltitude: Integer;
    //fZenithDistance: Double;
    fSunrise: TDateTime;
    fSunset: TDateTime;
    fSunNoon: TDateTime;
    fTypeSun: TSunrise;
    fOnchange: TNotifyEvent;
    procedure ParametersChanged(Sender: TObject);
    procedure SetTypeSun(value: TSunRise);
    procedure SetSundate(Value: TDateTime);
    procedure SetTimeZone(Value: Double);
    procedure SetDST(Value: Double);
    procedure SetLatitude(Value: Double);
    procedure SetLongitude(Value: Double);
    procedure SetAltitude(Value: Integer);
    function calcSunrise(index: Integer): TDateTime;
    function calcSolNoonUTC(t, longitude: double): double;
    function calcTimeJulianCent(jd: double): double;
    function calcJDFromJulianCent(t: double): double;
    function calcEquationOfTime(t: double): double;
    function calcObliquityCorrection(t: double):double;
    function calcMeanObliquityOfEcliptic(t: double): double;
    function calcGeomMeanLongSun(t: double): double;
    function calcEccentricityEarthOrbit(t: double): double;
    function calcGeomMeanAnomalySun(t: double): double;
    function calcSunDeclination(t: double): double;
    function calcSunApparentLong(t: double): double;
    function calcSunTrueLong(t: double): double;
    function calcSunEqOfCenter(t: double): double;
    function calcHourAngleSunrise(lat, solarDec: double; TypeSun: TSunRise=standard): double;
    function UTCMinutesToUTCTime(const Minutes: Double): TDateTime;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Version: String read fVersion;

    property Sunrise: TDateTime read fSunrise;//calcSunrise;
    property Sunset: TDateTime read fSunset;
    property SunNoon: TDateTime read fSunNoon;

  published
    property Sundate: TDateTime read fSundate write SetSundate;
    property TimeZone: Double read fTimeZone write SetTimeZone;
    property DST: Double read fDST write setDST;
    property Latitude: Double read fLatitude write SetLatitude;
    property Longitude: Double read fLongitude write SetLongitude;
    property Altitude: Integer read fAltitude write SetAltitude;       // Not yet implemented
    property TypeSun: TSunrise read fTypeSun write SetTypeSun;
    //property ZenithDistance: Double read fZenithDistance; // write SetZenithDistance;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I suntime_icon.lrs}
  RegisterComponents('lazbbAstroComponents',[TSuntime]);
end;

constructor TSuntime.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fVersion:= '2.0';
  fTypeSun:= standard;
end;


destructor TSuntime.Destroy;
begin
  inherited Destroy;
end;

procedure TSuntime.SetTypeSun(value: TSunRise);
begin
 if fTypeSun <> Value then
 begin
   fTypeSun:= Value;
   ParametersChanged(nil);
 end;
end;

procedure TSuntime.SetSundate(Value: TDateTime);
begin
  if fsundate <> Value then
  begin
    fSundate := Value;
    ParametersChanged(nil);
  end;
end;

procedure TSuntime.SetTimeZone(Value: Double);
begin
  if (TimeZone <> Value) and (Value >= -12) and (Value <= +12) then
  begin
    fTimeZone := Value;
    ParametersChanged(nil);
  end;
end;

procedure TSuntime.SetDST(Value: Double);
begin
  if fDST<>value then
  begin
    fDST := Value;
    ParametersChanged(nil);
  end;
end;

procedure TSuntime.SetLatitude(Value: Double);
begin
  if fLatitude<>value then
  begin
    fLatitude:= Value;
    ParametersChanged(nil);
  end;
end;

procedure TSuntime.SetLongitude(Value: Double);
begin
  if fLongitude<>value then
  begin
    fLongitude:= Value;
    ParametersChanged(nil);
  end;
end;

procedure TSuntime.SetAltitude(Value: Integer);
begin
  if fAltitude<>value then
  begin
    fAltitude:= Value;
    ParametersChanged(nil);
  end;
end;


procedure TSuntime.ParametersChanged(Sender: TObject);
begin
  fSunrise:= calcSunrise(0);
  fSunset:= calcSunrise(1);
  fSunNoon:= calcSunrise(2);
  if Assigned(FOnChange) then FOnChange(Self);
end;


//***********************************************************************/
// calcSunrise  : calculate the local time of sunrise/sunset
// index 0 sunrise, 1 sunset and 2 noon
// Return value: local time
//***********************************************************************/

function TSuntime.calcSunrise(index: Integer): TDateTime;
var
  r: integer;
  JD, t: double;
  noonmin, tnoon, eqTime, solarDec, hourAngle: double;
  delta, timeDiff,timeUTC, newt : double;
  hr, mn, y, m, d : Integer;
  longit: Double;
begin
  // longitudes are inversed (east negative)
  longit:= -1*flongitude;
  JD:= DateTimeToJulianDate(fSundate);
  t:= calcTimeJulianCent(JD);
  noonmin:= calcSolNoonUTC(t, longit);      // Find the time of solar noon at the location
  if index=2 then
  begin
    result:= UTCMinutesToUTCTime(noonmin)+(fTimeZone+fDST)/24;   // update noon value
    exit;
  end;
  tnoon:= calcTimeJulianCent (JD+noonmin/1440.0);
  // *** First pass to approximate sunrise or sunset (using solar noon)
  // check if sunrise or sunset
  if index= 0 then r:= 1 else r:= -1;
  eqTime:= calcEquationOfTime(tnoon);
  solarDec:= calcSunDeclination(tnoon);
  hourAngle:= calcHourAngleSunrise(flatitude, solarDec, fTypeSun)*r;
  delta:= longit - radToDeg(hourAngle);
  timeDiff:= 4 * delta;	// in minutes of time
  timeUTC:= 720 + timeDiff - eqTime;	// in minutes
  // *** Second pass includes fractional jday in gamma calc
  newt:= calcTimeJulianCent(calcJDFromJulianCent(t) + timeUTC/1440.0);
  eqTime:= calcEquationOfTime(newt);
  solarDec:= calcSunDeclination(newt);
  hourAngle:= calcHourAngleSunrise(flatitude, solarDec, fTypeSun)*r;
  delta:= longit - radToDeg(hourAngle);
  timeDiff:= 4 * delta;
  timeUTC:= 720 + timeDiff - eqTime; // in minutes
  hr:= floor(timeUTC /60);
  mn:= floor(timeUTC- hr*60);
  y:= YearOf(fSundate);
  m:= MonthOf(fSundate);
  d:= DayOfTheMonth(fSundate);
  result:= EncodeDateTime(y,m,d,hr,mn,0,0)+(fTimeZone+fDST)/24;
end;

//***********************************************************************
// calcSolNoonUTC
//   calculate the Universal Coordinated Time (UTC) of solar
//		noon for the given day at the given location on earth
//   t : number of Julian centuries since J2000.0
//   longitude : longitude of observer in degrees
// Return value: time in minutes from zero Z
//***********************************************************************/

function TSuntime.calcSolNoonUTC(t, longitude: double): double;
var
  tnoon, eqTime, solNoonUTC, newt: double;
begin
  //First pass uses approximate solar noon to calculate eqtime
  tnoon:= calcTimeJulianCent(calcJDFromJulianCent(t) + longitude/360.0);
  eqTime:= calcEquationOfTime(tnoon);
  solNoonUTC:= 720 + (longitude * 4) - eqTime; // min
  newt:= calcTimeJulianCent(calcJDFromJulianCent(t) -0.5 + solNoonUTC/1440.0);
  eqTime:= calcEquationOfTime(newt);
  // var solarNoonDec = calcSunDeclination(newt);
  solNoonUTC:= 720 + (longitude * 4) - eqTime; // min
  result:= solNoonUTC;
end;

//***********************************************************************/
//* Name:    calcTimeJulianCent
//* Type:    Function
//* Purpose: convert Julian Day to centuries since J2000.0.
//* Arguments:
//*   jd : the Julian Day to convert
//* Return value:
//*   the T value corresponding to the Julian Day
//***********************************************************************/

function TSuntime.calcTimeJulianCent(jd: double): double;
begin
  result:=  (jd-2451545.0)/36525.0;
end;

//***********************************************************************/
//* Name:    calcJDFromJulianCent
//* Type:    Function
//* Purpose: convert centuries since J2000.0 to Julian Day.
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   the Julian Day corresponding to the t value
//***********************************************************************/

function TSuntime.calcJDFromJulianCent(t: double): double;
var
  JD: double;
begin
  JD:= (t*36525.0) + 2451545.0;
result:= JD;
end;

//***********************************************************************/
//* Name:    calcEquationOfTime
//* Type:    Function
//* Purpose: calculate the difference between true solar time and mean solar time
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   equation of time in minutes of time
//***********************************************************************/

function TSuntime.calcEquationOfTime(t: double): double;
var
 epsilon, l0, e, m, y : double;
 sin2l0, sinm, cos2l0, sin4l0, sin2m : double;
 Etime: Double;
begin
  epsilon:= calcObliquityCorrection(t);
  l0:= calcGeomMeanLongSun(t);
  e:= calcEccentricityEarthOrbit(t);
  m:= calcGeomMeanAnomalySun(t);
  y:= sqr(tan(degToRad(epsilon)/2.0));
  sin2l0:= sin(2.0 * degToRad(l0));
  sinm:= sin(degToRad(m));
  cos2l0:= cos(2.0 * degToRad(l0));
  sin4l0:= sin(4.0 * degToRad(l0));
  sin2m:= sin(2.0 * degToRad(m));
  Etime:= y * sin2l0 - 2.0 * e * sinm + 4.0 * e * y * sinm * cos2l0 - 0.5 * y * y * sin4l0 - 1.25 * e * e * sin2m;
  result:=  radToDeg(Etime)*4.0;	// in minutes of time
end;


//***********************************************************************/
//* Name:    calcObliquityCorrection
//* Type:    Function
//* Purpose: calculate the corrected obliquity of the ecliptic
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   corrected obliquity in degrees
//***********************************************************************/

function TSuntime.calcObliquityCorrection(t: double):double;
var
 e0, omega, e: double;
begin
  e0:= calcMeanObliquityOfEcliptic(t);
  omega:= 125.04 - 1934.136 * t;
  e:= e0 + 0.00256 * cos(degToRad(omega));
  result:= e;		// in degrees
end;

//***********************************************************************/
//* Name:    calcMeanObliquityOfEcliptic
//* Type:    Function
//* Purpose: calculate the mean obliquity of the ecliptic
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   mean obliquity in degrees
//***********************************************************************/

function TSuntime.calcMeanObliquityOfEcliptic(t: double): double;
var
  seconds, e0 : double;
begin
  seconds:= 21.448 - t*(46.8150 + t*(0.00059 - t*(0.001813)));
  e0:= 23.0 + (26.0 + (seconds/60.0))/60.0;
  result:= e0;    //in degrees
end;


//***********************************************************************/
//* Name:    calGeomMeanLongSun
//* Type:    Function
//* Purpose: calculate the Geometric Mean Longitude of the Sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   the Geometric Mean Longitude of the Sun in degrees
//***********************************************************************/

function TSuntime.calcGeomMeanLongSun(t: double): double;
var
  L0: double;
begin
  L0:= 280.46646 + t * (36000.76983 + 0.0003032 * t);
  while(L0 > 360.0) do L0:= L0 - 360.0;
  while(L0 < 0.0) do L0:= L0 + 360.0;
  result:= L0; // in degrees
end;

//***********************************************************************/
//* Name:    calcEccentricityEarthOrbit
//* Type:    Function
//* Purpose: calculate the eccentricity of earth's orbit
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   the unitless eccentricity
//***********************************************************************/

function TSuntime.calcEccentricityEarthOrbit(t: double): double;
var
  e: double;
begin
  e:= 0.016708634 - t * (0.000042037 + 0.0000001267 * t);
  result:= e;		// unitless
end;

//***********************************************************************/
//* Name:    calGeomAnomalySun
//* Type:    Function
//* Purpose: calculate the Geometric Mean Anomaly of the Sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   the Geometric Mean Anomaly of the Sun in degrees
//***********************************************************************/

function TSuntime.calcGeomMeanAnomalySun(t: double): double;
var
  M: double;
begin
  M:= 357.52911 + t * (35999.05029 - 0.0001537 * t);
  result:= M;		// in degrees
end;

//***********************************************************************/
//* Name:    calcSunDeclination
//* Type:    Function
//* Purpose: calculate the declination of the sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   sun's declination in degrees
//***********************************************************************/

function TSuntime.calcSunDeclination(t: double): double;
var
  e, lambda, sint, theta: double;
begin
  e:= calcObliquityCorrection(t);
  lambda:= calcSunApparentLong(t);
  sint:= sin(degToRad(e)) * sin(degToRad(lambda));
  theta:= radToDeg(arcsin(sint));
  result:= theta;		// in degrees
end;

//***********************************************************************/
//* Name:    calcSunApparentLong
//* Type:    Function
//* Purpose: calculate the apparent longitude of the sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   sun's apparent longitude in degrees
//***********************************************************************/

function TSuntime.calcSunApparentLong(t: double): double;
var
  o, omega, lambda: double;
begin
  o:= calcSunTrueLong(t);
  omega:= 125.04 - 1934.136 * t;
  lambda:= o - 0.00569 - 0.00478 * sin(degToRad(omega));
result:= lambda;		// in degrees
end;

//***********************************************************************/
//* Name:    calcSunTrueLong
//* Type:    Function
//* Purpose: calculate the true longitude of the sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   sun's true longitude in degrees
//***********************************************************************/

function TSuntime.calcSunTrueLong(t: double): double;
var
  l0, c, O: double;
begin
  l0:= calcGeomMeanLongSun(t);
  c:= calcSunEqOfCenter(t);
  O:= l0 + c;
  result:= O;		// in degrees
end;

//***********************************************************************/
//* Name:    calcSunEqOfCenter
//* Type:    Function
//* Purpose: calculate the equation of center for the sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   in degrees
//***********************************************************************/

function TSuntime.calcSunEqOfCenter(t: double): double;
var
  m, mrad, sinm, sin2m, sin3m, C : double;
begin
  m:= calcGeomMeanAnomalySun(t);
  mrad:= degToRad(m);
  sinm:= sin(mrad);
  sin2m:= sin(mrad+mrad);
  sin3m:= sin(mrad+mrad+mrad);
  C:= sinm * (1.914602 - t * (0.004817 + 0.000014 * t)) + sin2m * (0.019993 - 0.000101 * t) + sin3m * 0.000289;
  Result:= C;		// in degrees
end;

//***********************************************************************/
//* Name:    calcHourAngleSunrise
//* Purpose: calculate the hour angle of the sun at sunrise for the latitude
//* Arguments:
//*   lat : latitude of observer in degrees
//*   solarDec : declination angle of sun in degrees
//*   TypSun : standard, civil, nautic, astro
//* Return value:
//*   hour angle of sunrise in radians
//***********************************************************************/

function TSuntime.calcHourAngleSunrise(lat, solarDec: double; TypeSun: TSunRise=standard): double;
var
  latRad, sdRad, HA: double;
  ZenithDistance: Double;
const
  aSunTypDeg: array [0..4] of Double = (90.83333, 96, 102, 108, 90);

begin
  try
    ZenithDistance:= aSunTypDeg[Ord(TypeSun)];
  except
    ZenithDistance:= 90.83333;
  end;
  latRad:= degToRad(lat);
  sdRad:= degToRad(solarDec);
  HA:= (arccos(cos(degToRad(ZenithDistance))/(cos(latRad)*cos(sdRad))-tan(latRad) * tan(sdRad)));
  Result:= HA;		// in radians
end;

function TSuntime.UTCMinutesToUTCTime(const Minutes: Double): TDateTime;
begin
  Result := Trunc(fSundate) + (Minutes / 60 ) / 24;
end;


end.
