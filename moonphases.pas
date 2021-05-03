unit Moonphases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TMoonRecord = record
    MDays: TDateTime;
    MType: string;
  end;

  TMoonDays = array[1..56] of TMoonRecord;

  TMoonphases = class(TComponent)
  private
    fversion: String;
    fMoondays: TMoonDays;
    fMoondate: TDateTime;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Moondate: TDateTime read fMoondate;
    property Moondays: TMoonDays read fMoondays;
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I moonphases_icon.lrs}
  RegisterComponents('lazbbAstroComponents',[TMoonphases]);
end;

constructor TMoonphases.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fVersion:= '1.0';

end;


destructor TMoonphases.Destroy;
begin
  inherited Destroy;
end;

function Get_MoonDays(gDate: TDateTime): TMoonDays;
// const du jour julien du 01-01-2000 à 12h TU
// ou 0h GMT 01-01-2000 12:00:00 TU 2451545 JJ
// const JJ2000 = 2451545;
var
  TUJrLun: TDateTime; K, TT: integer; TDatL, TxtL, THorL: string;
  AnDecim, T, T2, T3, Rd, AMSol, AMLun, LatLune, PhMoyL: double;
  NoLune, tLune, J, gLunes, PhLun, CurM, CurM2, HrLun, MnLun: byte;
  AnPh, MoPh, AJour, LunAnW, LunMsW, JrLunEntW: word;
  CptLMax, CptL, PentPhL, PentPhMoyL, PfracPhMoyL, Alpha, B, C, D, E: single;
  LunAn, LunMs, JrLun, JrLunFrac, TotHeu, TotMin, TotSec: single;
  ListDatLun: array[1..56] of string;
  gNbrLune: array[1..56] of byte;
  AnBis,  Found : boolean;

begin
{  DecodeDate(gDate, AnPh, MoPh, AJour);
  gLunes:= 0;
  AJour:= 1;  // AJour = 1 pour éviter AJour + 3 > 31
  if MoPh = 0 then begin MoPh:= 12; AnPh:= AnPh - 1; end;
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
    TUJrLun:= EncodeTime(HrLun, MnLun, 0, 0);
    //THorL:= FormatDateTime('hh"h "mm',TUJrLun);
    PhLun:= 0;
    if CptL >= 0 then
    begin
      if frac(CptL) =  0.0   then PhLun:= 6;// NL
      if frac(CptL) =  0.125 then PhLun:= 5;
      if frac(CptL) =  0.25  then PhLun:= 4;// DQ
      if frac(CptL) =  0.375 then PhLun:= 3;
      if frac(CptL) =  0.5   then PhLun:= 2;// PL
      if frac(CptL) =  0.625 then PhLun:= 1;
      if frac(CptL) =  0.75  then PhLun:= 8;// PQ
      if frac(CptL) =  0.875 then PhLun:= 7;
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
    TT:= PhLun;
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
    // CurM2
    // Phase de lune paire  (pl, pq, dq ou nl)
    if (TT <> 0) and ((TT div 2) = (TT / 2)) then
    begin
      if (EncodeDate(LunAnW,LunMsW,JrLunEntW) <= EncodeDate(AnPh,MoPh,AJour + 3))
         and (PhLun <> 0) then CurM2:= PhLun;
    end;
    // CurM
    if (EncodeDate(LunAnW,LunMsW,JrLunEntW) <= EncodeDate(AnPh,MoPh,AJour + 1))
        and (PhLun <> 0 ) then CurM:= PhLun;
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
      ListDatLun[gLunes]:= TDatL;
      //ListHeuLun[gLunes]:= THorL;
      MoonDays[gLunes].MDays:= EncodeDate(LunAnW,LunMsW,JrLunEntW)+EncodeTime(HrLun,MnLun,0,0);// date de lune
      //MoonDays[gLunes].MTime:= EncodeTime(HrLun,MnLun,0,0);        // horaire de lune
      gNbrLune[gLunes]:= tLune;
    end; // else exit;
    CptL:= CptL + 0.250;
  end;
    for J:= 1 To 56 do
    begin
      case gNbrLune[J] of
        35: TxtL:= 'PL';   //36:   '1
        37: TxtL:= 'DQ';   //38:   '3
        39: TxtL:= 'NL';   //40:   '5
        41: TxtL:= 'PQ';   //42:   '7
      end;
      MoonDays[J].MType:= TxtL; // type de lune
    end;
  //end;
  result:= MoonDays;   }
end;

end.
