{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit atgauge_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  ATGauge, atgauge_register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('atgauge_register', @atgauge_register.Register);
end;

initialization
  RegisterPackage('atgauge_package', @Register);
end.
