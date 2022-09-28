unit Component1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TComponent1 = class(TComponent)
  private
    fProdStr: TStrings ;
    procedure SetProdStr(const AValue: TStrings);
  protected

  public
    constructor Create(aOwner: Tcomponent); override;
    destructor Destroy; override;
  published
    property ProdStr: TStrings read fProdStr write SetProdStr;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BB',[TComponent1]);
end;

constructor TComponent1.Create(aOwner: Tcomponent);
var
  i: Integer;

begin
  inherited Create(aOwner);
  fProdStr:= TStringList.Create;
end;

destructor TComponent1.Destroy;
begin
  FreeAndNil(fProdStr);
  inherited;
end;

procedure TComponent1.SetProdStr(const aValue: TStrings);
begin
   if Assigned(fProdStr) then
    fProdStr.Assign(AValue);
end;

end.
