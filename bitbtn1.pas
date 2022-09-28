unit BitBtn1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons;

type
  TBitBtn1 = class(TBitBtn)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I bitbtn1_icon.lrs}
  RegisterComponents('lazbbComponents',[TBitBtn1]);
end;

end.
