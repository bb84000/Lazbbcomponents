{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazbbastrocomponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  Suntime, Moonphases, Seasons, Easter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Suntime', @Suntime.Register);
  RegisterUnit('Moonphases', @Moonphases.Register);
  RegisterUnit('Seasons', @Seasons.Register);
  RegisterUnit('Easter', @Easter.Register);
end;

initialization
  RegisterPackage('lazbbastrocomponents', @Register);
end.
