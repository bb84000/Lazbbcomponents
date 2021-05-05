//******************************************************************************
// Moonphases unit : compute moon phases times
// original author : alantell - november 2004 from astronomy books
// Lazarus adaptation and improvements : bb - sdtp - May 2021
//
// Parameters:
//   Moonyear (Integer) : Selected year for moon phases
//   Moondate (TDateTime) : Selected date for the function isMoon
// Properties
//   isMoon (TMoonRecord) : Is there a moon phase on the day Moondate
//   Crescent (Boolean) : True, the list has 8 phases and has 112 dates and hours
//                        False, the list has 4 phases and has 56 date and hours
//   Moondays (TMoonDays) : Array of 56 TMoonRecords containing moon pahses for the year Moonyear
//   MoonImages (TImageList): List of 8 moon images relative to MIndex value
// Type
//   TMoonRecord
//     MDays (TDateTime) : Moonphase date and hour, or InvalidDate (01/01/0001) if not found
//     MType (String) : New Moon, Waxing crescent, First quarter, Waxing gibbous,
//                      Full moon, Waning gibbous, third/last quarter, Waning crescent
//    MIndex (Integer) : 0 (NM) to 7 (WC) Useful to acces MoonImages and/or customize application
//**************************************************************************************************



unit Moonphases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  TMoonRecord = record
    MDays: TDateTime;
    MType: string;
    MIndex: Integer;
  end;

  TMoonDays = array of TMoonRecord;

  TMoonphases = class(TComponent)
  private
    fversion: String;
    fMoondays: TMoonDays;
    fMoonyear: Integer;
    fMoondate: TDateTime;
    fCrescents: Boolean;
    fready: Boolean;
    fMoonImages: TImageList;
    procedure Get_MoonDays;
    procedure setMoonDate(value: TDateTime);
    procedure setMoonYear(value: Integer);
    procedure SetCrescents(value: Boolean);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function isMoon:  TMoonRecord;
    property Moondate: TDateTime read fMoondate write SetMoondate;
    property Moondays: TMoonDays read fMoonDays;
    property MoonImages: TImageList read fMoonImages; // write setMoonImages;
  published
    property Moonyear: Integer read fMoonyear write setMoonYear;
    property Crescents: Boolean read FCrescents write SetCrescents;
  end;

  const
    InvalidDate= -693593; // 1st january 0001

procedure Register;

implementation

procedure Register;
begin
  {$I moonphases.lrs}
  RegisterComponents('lazbbAstroComponents',[TMoonphases]);
end;

constructor TMoonphases.Create(AOwner: TComponent);
const
  simgArr: array of String = ('new_moon', 'waxing_crescent', 'first_quarter', 'waxing_gibbous', 'full_moon', 'waning_gibbous', 'last_quarter', 'waning_crescent');
var
  i: Integer;
begin
  inherited Create(AOwner);
  {$I moonphases.lrs}
  fVersion:= '1.0';
  fMoonYear:= CurrentYear;
  fMoonImages:= TimageList.Create(self);
  fMoonImages.Height:= 44;
  fMoonImages.Width:= 44;
  For i:= 0 to 7 do
    fmoonImages.AddLazarusResource(simgArr[i]);
  fReady:= false;
end;


destructor TMoonphases.Destroy;
begin
  inherited Destroy;
end;

procedure TMoonphases.setMoonDate(value: TDateTime);
begin
  if fMoonDate<>value then
  begin
    fMoonDate:= value;
  end;
end;

procedure TMoonphases.setMoonYear(value: Integer);
begin
  if fMoonYear<>value then
  begin
    fMoonYear:= value;
    if not (csDesigning in ComponentState) then Get_MoonDays;
  end;
end;

procedure TMoonphases.SetCrescents(value: Boolean);
begin
  if fCrescents<> value then
  fCrescents:= value;
  if not (csDesigning in ComponentState) then Get_MoonDays;
end;

procedure TMoonphases.Get_MoonDays;
// const du jour julien du 01-01-2000 à 12h TU
// ou 0h GMT 01-01-2000 12:00:00 TU 2451545 JJ
// const JJ2000 = 2451545;
var
  TDatL, TxtL: string;
  AnDecim, T, T2, T3, Rd, AMSol, AMLun, LatLune, PhMoyL: double;
  NoLune, tLune, J, gLunes, PhLun, HrLun, MnLun: byte;
  AnPh, MoPh, AJour, LunAnW, LunMsW, JrLunEntW: word;
  CptLMax, CptL, PentPhL, PentPhMoyL, PfracPhMoyL, Alpha, B, C, D, E: single;
  LunAn, LunMs, JrLun, JrLunFrac, TotHeu, TotMin, TotSec: single;
  ListDatLun: array of string;
  //ListHeuLun: array[1..56] of string;
  gNbrLune: array of byte;
  AnBis,  Found : boolean;
  NumDays: Integer;
  interval: Double;
  ndx: Integer;
begin
  // Avoid trouble on the beginning of year, substreact some days
  AnPh:= fMoonYear-1;
  MoPh:= 12;
  Ajour:= 1;   // avoid error in february when using ajour+3
  gLunes:= 0;
  if fCrescents then
  begin
    Numdays:= 112;
    interval:= 0.125;
  end else
  begin
    numdays:= 56;
    interval:= 0.250;
  end;
  SetLength(fMoondays, Numdays);
  SetLength(ListDatLun, Numdays);
  SetLength(gNbrLune, NumDays);
  CptLMax:= 14; // définit le nb phase de lune ex: 13 lunes => 56 phases
  // valeur année décimale par mois si année bissextile ou pas
  AnBis:= ((AnPh Mod 4)= 0);    // si = 0 année bissextile
  if AnBis then begin
      case MoPh of
       1: AnDecim:= 4.24375935815675E-02;
       2: AnDecim:= 0.124574871481376;
       3: AnDecim:= 0.20534319474952;
       4: AnDecim:= 0.288849427280992;
       5: AnDecim:= 0.372355659812463;
       6: AnDecim:= 0.455861892343935;
       7: AnDecim:= 0.539368124875406;
       8: AnDecim:= 0.624243312038541;
       9: AnDecim:= 0.707749544570013;
      10: AnDecim:= 0.791255777101484;
      11: AnDecim:= 0.874762009632956;
      12: AnDecim:= 0.958268242164428;
      end;
    end
  else begin
    case MoPh of
       1: AnDecim:= 4.24375935815675E-02;
       2: AnDecim:= 0.123205916849712;
       3: AnDecim:= 0.203974240117857;
       4: AnDecim:= 0.287480472649328;
       5: AnDecim:= 0.3709867051808;
       6: AnDecim:= 0.454492937712271;
       7: AnDecim:= 0.537999170243743;
       8: AnDecim:= 0.622874357406878;
       9: AnDecim:= 0.706380589938349;
      10: AnDecim:= 0.789886822469821;
      11: AnDecim:= 0.873393055001292;
      12: AnDecim:= 0.956899287532764;
    end;
  end;
  // calcul nb de lunaison CptL nb de lunes en 1 an 12.3685 => 365.25 / 29.53058
  // nombre de lunaisons depuis le 1/1/1900
  CptL:= Trunc(((AnPh + AnDecim) - 1900) * 12.3685);
  CptLMax:= CptL + CptLMax;

//CalculDesPh:
  while  (CptL < CptLMax) do
  begin
    T:= CptL / 1236.85; T2:= T*T; T3:= T*T*T; Rd:= PI / 180;
    // anomalie moyenne du soleil : AMSol
    AMSol:= 359.2242 + (29.10535608 * CptL) - (0.0000333 * T2) - (0.00000347 * T3);
    if AMSol > 360 then AMSol:= frac(AMSol/360) * 360; // intervalle 0-360°
    // anomalie moyenne de la lune : AMLun
    AMLun:= 306.0253 + (385.81691806 * CptL) + (0.0107306 * T2) + (0.00001236 * T3);
    if AMLun > 360 then AMLun:= frac(AMLun/360) * 360; // intervalle 0-360°
    // Latitude de la lune
    LatLune:= 21.2964 + (390.67050646 * CptL)-(0.0016528 * T2)-(0.00000239 * T3);
    if LatLune > 360 then LatLune:= frac(LatLune/360) * 360; // intervalle 0-360°
    // Phase moyenne de la Lune 2415020.75933 1er jour julien 1/1/-4711
    PhMoyL:= 2415020.75933 + (29.53058868 * CptL) + (0.0001178*T2)-(0.000000155*T3);
    PhMoyL:= PhMoyL + (0.00033*Sin((Rd*166.56) + (Rd*132.87)*T)-((Rd*0.009173*T2)));
    // degrés en radian
    AMSol:= AMSol * Rd;
    AMLun:= AMLun * Rd;
    LatLune:= LatLune * Rd;
    // correction de la phase vraie pour nouvelle et pleine lune
    if (frac(CptL) = 0.0) Or (frac(CptL) = 0.5) Or (frac(CptL) = -0.5) then
    begin
      PhMoyL:= PhMoyL + ((0.1734 - 0.000393 * T) * Sin(AMSol));
      PhMoyL:= PhMoyL + (0.0021 * Sin(2 * AMSol));
      PhMoyL:= PhMoyL - (0.4068 * Sin(AMLun));
      PhMoyL:= PhMoyL + (0.0161 * Sin(2 * AMLun));
      PhMoyL:= PhMoyL - (0.0004 * Sin(3 * AMLun));
      PhMoyL:= PhMoyL + (0.0104 * Sin(2 * LatLune));
      PhMoyL:= PhMoyL - (0.0051 * Sin(AMSol + AMLun));
      PhMoyL:= PhMoyL - (0.0074 * Sin(AMSol - AMLun));
      PhMoyL:= PhMoyL + (0.0004 * Sin((2 * LatLune) + AMSol));
      PhMoyL:= PhMoyL - (0.0004 * Sin((2 * LatLune) - AMSol));
      PhMoyL:= PhMoyL - (0.0006000001 * Sin((2 * LatLune) + AMLun));
      PhMoyL:= PhMoyL + (0.001 * Sin((2 * LatLune) - AMLun));
      PhMoyL:= PhMoyL + 0.0005 * Sin(AMSol + (2 * AMLun));
    end
    else begin
      // correction de la phase vraie pour premier et dernier quartier lune
      PhMoyL:= PhMoyL + (0.1721 - 0.0004 * T) * Sin(AMSol);
      PhMoyL:= PhMoyL + 0.0021 * Sin(2 * AMSol);
      PhMoyL:= PhMoyL - 0.628  * Sin(AMLun);
      PhMoyL:= PhMoyL + 0.0089 * Sin(2 * AMLun);
      PhMoyL:= PhMoyL - 0.0004 * Sin(3 * AMLun);
      PhMoyL:= PhMoyL + 0.0079 * Sin(2 * LatLune);
      PhMoyL:= PhMoyL - 0.0119 * Sin(AMSol + AMLun);
      PhMoyL:= PhMoyL - 0.0047 * Sin(AMSol - AMLun);
      PhMoyL:= PhMoyL + 0.0003 * Sin(2 * LatLune + AMSol);
      PhMoyL:= PhMoyL - 0.0004 * Sin(2 * LatLune - AMSol);
      PhMoyL:= PhMoyL - 0.0006000001 * Sin(2 * LatLune + AMLun);
      PhMoyL:= PhMoyL + 0.0021 * Sin(2 * LatLune - AMLun);
      PhMoyL:= PhMoyL + 0.0003 * Sin(AMSol + 2 * AMLun);
      PhMoyL:= PhMoyL + 0.0004 * Sin(AMSol - 2 * AMLun);
      PhMoyL:= PhMoyL - 0.0003 * Sin(2 * AMSol - AMLun);
      // ajustement suivant le quartier
      if (CptL >= 0)  then
      begin
        if (frac(CptL) = 0.25) then PhMoyL:= PhMoyL + 0.0028 - 0.0004 * Cos(AMSol) + 0.0003 * Cos(AMLun);     //1er quartier
        if (frac(CptL) = 0.75) then PhMoyL:= PhMoyL - 0.0028 + 0.0004 * Cos(AMSol) - 0.0003 * Cos(AMLun);     // dernier quartier
      end else
      begin
        if (frac(CptL) = -0.25) then PhMoyL:= PhMoyL - 0.0028 + 0.0004 * Cos(AMSol) - 0.0003 * Cos(AMLun);
        if (frac(CptL) = -0.75) then PhMoyL:= PhMoyL + 0.0028 - 0.0004 * Cos(AMSol) + 0.0003 * Cos(AMLun);
      end;
    end;
    // calcul des dates de lune calendrier
    PhMoyL:= PhMoyL + 0.5;
    PentPhMoyL:= Trunc(PhMoyL);
    PfracPhMoyL:= frac(PhMoyL);
    if PentPhMoyL <  2299161 then PentPhL:= PentPhMoyL
    else
    begin
      ALPHA:= Trunc((PentPhMoyL - 1867216.25) / 36524.25);
      PentPhL:= PentPhMoyL + 1 + ALPHA - Trunc(ALPHA / 4);
    end;
    B:= PentPhL + 1524;
    C:= Trunc((B - 122.1) / 365.25);
    D:= Trunc(365.25 * C);
    E:= Trunc((B - D) / 30.6001);
    JrLun:= B - D - Trunc(30.6001 * E) + PfracPhMoyL;
    LunMs:= 1; // initialisation
    if E < 13.5 then LunMs:= E - 1;
    if E > 13.5 then LunMs:= E - 13;
    if LunMs > 2.5 then LunAn:= C - 4716;
    if LunMs < 2.5 then LunAn:= C - 4715;
    LunAnW:= Trunc(LunAn);
    LunMsW:= Trunc(LunMs);
    JrLunEntW:= Trunc(JrLun);
    JrLunFrac:= frac(JrLun);
    TotSec:= JrLunFrac * 86400;
    TotHeu:= (TotSec / 3600);
    HrLun:= Trunc(TotHeu);
    TotMin:= frac(TotHeu) * 60;
    MnLun:= Trunc(TotMin);
    // horaire de la lune
    //TUJrLun:= EncodeTime(HrLun, MnLun, 0, 0);
    //THorL:= FormatDateTime('hh"h "mm',TUJrLun);
    PhLun:= 0;
    if CptL >= 0 then
    begin
      if frac(CptL) =  0.0   then PhLun:= 6;  // NL  Nouvelle lune
      if frac(CptL) =  0.125 then PhLun:= 5;  // DC  Dernier croissant
      if frac(CptL) =  0.25  then PhLun:= 4;  // DQ  Dernier quartier
      if frac(CptL) =  0.375 then PhLun:= 3;  // GD  Gibbeuse décroissante
      if frac(CptL) =  0.5   then PhLun:= 2;  // PL  Pleine Lune
      if frac(CptL) =  0.625 then PhLun:= 1;  // GC  Gibbeuse croissante
      if frac(CptL) =  0.75  then PhLun:= 8;  // PQ  Premier quartier
      if frac(CptL) =  0.875 then PhLun:= 7;  // PC  Premier croissant
    end
    else begin
      if frac(CptL) = -0.875 then PhLun:= 7;
      if frac(CptL) = -0.75  then PhLun:= 8;// PQ
      if frac(CptL) = -0.625 then PhLun:= 1;
      if frac(CptL) = -0.5   then PhLun:= 2;// PL
      if frac(CptL) = -0.375 then PhLun:= 3;
      if frac(CptL) = -0.25  then PhLun:= 4;// DQ
      if frac(CptL) = -0.125 then PhLun:= 5;
      if frac(CptL) =  0.0   then PhLun:= 6;// NL
    end;
    try
      EncodeDate(LunAnW,LunMsW,JrLunEntW); // jour de lune
    except
      MessageDlg('pb1' + inttostr(LunAnW) + inttostr(LunMsW) + inttostr(JrLunEntW),
                mtInformation,[mbOk], 0);
    end;
    try
      EncodeDate(LunAnW,LunMsW,JrLunEntW); // jour de lune
    except
      MessageDlg('pb2' + inttostr(AnPh) +  inttostr(MoPh) + inttostr(AJour),
                mtInformation,[mbOk], 0);
    end;
    Found:= False;
    TDatL:= DateToStr(EncodeDate(LunAnW,LunMsW,JrLunEntW)); // jour de lune

    NoLune:= PhLun;
    case NoLune of
      1: tLune:= 36;
      2: tLune:= 35; // PL
      3: tLune:= 42;
      4: tLune:= 41; // PQ
      5: tLune:= 40;
      6: tLune:= 39; // NL
      7: tLune:= 38;
      8: tLune:= 37; // DQ
    end;
    // enregistrement des jours de lune
    for J:= gLunes downTo 1 do
      if ListDatLun[J] = TDatL then
      begin
        Found:= True;
        //Exit;
      end;
    // End for
    if not Found then
    begin
      gLunes:= gLunes + 1;
      ListDatLun[gLunes-1]:= TDatL;
      //ListHeuLun[gLunes]:= THorL;
      fMoonDays[gLunes-1].MDays:= EncodeDate(LunAnW,LunMsW,JrLunEntW)+EncodeTime(HrLun,MnLun,0,0);// date de lune
      //MoonDays[gLunes].MTime:= EncodeTime(HrLun,MnLun,0,0);        // horaire de lune
      gNbrLune[gLunes-1]:= tLune;
    end; // else exit;
    CptL:= CptL + interval;
  end;
  //simgArr: array of String = ('NL', 'PC', 'PQ', 'GC', 'PL', 'GD', 'DQ', 'DC');
  ndx:= -1;
  for J:= 0 To NumDays-1 do
  begin
    case gNbrLune[J] of
        35: begin
              TxtL:= 'Full moon';
              ndx:= 4;
            end;
        36: begin
              TxtL:= 'Waning gibbous';
              ndx:= 5;
            end;
        37: begin
              TxtL:= 'last quarter';
              ndx:= 6;
            end;
        38: begin
              TxtL:= 'Waning crescent';
              ndx:= 7;
            end;
        39: begin
              TxtL:= 'New moon';
              ndx:= 0;
            end;
        40: begin
              TxtL:= 'Waxing crescent';
              ndx:= 1;
            end;
        41: begin
              TxtL:= 'First quarter';
              ndx:= 2;
            end;
        42: begin
              TxtL:= 'Waxing gibbous';
              ndx:= 3;
            end;
    end;
    fMoonDays[J].MType:= TxtL; // type de lune
    fMoonDays[J].MIndex:= ndx;
  end;
  fready:= true;
end;

// GetMoonday doit avoir été lancé !!!
// Retrouve le jour fMoonDate
function TMoonphases.isMoon: TMoonRecord;
var
  i: integer;
begin
  result.MDays:= InvalidDate;
  result.MType:= '';
  try
  if not fready then exit;
  for i:= 1 to 56 do
    if Trunc(fMoondate) = Trunc (fMoonDays[i].MDays) then result:= fMoonDays[i];
  except
  end;
end;


end.
