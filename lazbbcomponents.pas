{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazbbcomponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  lazbbcontrols, lazbbtrackbar, lazbbscrollcontrols, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lazbbcontrols', @lazbbcontrols.Register);
  RegisterUnit('lazbbtrackbar', @lazbbtrackbar.Register);
  RegisterUnit('lazbbscrollcontrols', @lazbbscrollcontrols.Register);
end;

initialization
  RegisterPackage('lazbbcomponents', @Register);
end.
