unit atgauge_register;

interface

uses
  SysUtils, Classes, Controls, LResources, atgauge;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TGauge]);
end;

initialization
  {$I res/icon.lrs}

end.
