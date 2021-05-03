{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazbbastrocomp;

{$warn 5023 off : no warning about unused units}
interface

uses
  Suntime, Moonphases, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Suntime', @Suntime.Register);
  RegisterUnit('Moonphases', @Moonphases.Register);
end;

initialization
  RegisterPackage('lazbbastrocomp', @Register);
end.
