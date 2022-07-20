unit MS_Types;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections;

type TPlayer = class
  name: string;
  location: string;
  public
    constructor Create;
    destructor Destroy(); override;
end;

var Players: TObjectList<TPlayer>;

implementation

constructor TPlayer.Create;
begin
  location:= 'void';
end;

destructor TPlayer.Destroy;
begin
  inherited;
end;

end.
