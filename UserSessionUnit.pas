unit UserSessionUnit;

{
  This is a DataModule where you can add components or declare fields that are specific to
  ONE user. Instead of creating global variables, it is better to use this datamodule. You can then
  access the it using UserSession.
}
interface

uses
  IWUserSessionBase,
  SysUtils,
  Classes,
  MS_Types,
  IW.Common.Lists,
  IW.HTTP.Cookie;

type
  TIWUserSession = class( TIWUserSessionBase )
  private
    { Private declarations }
    fNickname: string;
    fLoggedIn: boolean;
    fMoves: TStringList;
  public
    { Public declarations }
    score: integer;
    difficulty: integer;
    timestamp: TDateTime;
    procedure LoginPlayer(aNickname:string);
    procedure Set_Player_location(location:string);
    function is_logged_in: boolean;
    function GetCookieValue( sName : String ) : String;
    procedure SetCookie( CookieName, CookieValue : String; ExpireIn : Real );
    procedure ClearMoves;
    procedure AddMove(move: string);
    function FakeMoves: boolean;
    destructor Destroy;
  end;



implementation

{$R *.dfm}

procedure TIWUserSession.SetCookie( CookieName, CookieValue : String; ExpireIn : Real );
var ExpireOn : Real;
begin
  try
    if ExpireIn <> 0 then
      ExpireOn := Now + ExpireIn
    else
      ExpireOn := - 1; // session cookie
    WebApplication.Response.Cookies.Add( THTTPCookie.Create( CookieName, CookieValue, '/', ExpireOn ) );

  except
  end;
end;

procedure TIWUserSession.AddMove(move: string);
begin
  fMoves.Add(move);
end;

procedure TIWUserSession.ClearMoves;
begin
  if not Assigned(fMoves) then
    fMoves:= TStringList.Create;
end;

destructor TIWUserSession.Destroy;
begin
  fMoves.Free;

  inherited;
end;

function TIWUserSession.FakeMoves: boolean;
begin
  var expected_move_count:= difficulty * difficulty;
  result:= fMoves.Count<expected_move_count;
end;

function TIWUserSession.GetCookieValue( sName : String ) : String;
begin
  try
    result := WebApplication.Request.CookieFields.Values[ sName ];

  except
  end;
end;

function TIWUserSession.Is_logged_in: boolean;
begin
  result:= fLoggedIn;
end;

procedure TIWUserSession.LoginPlayer(aNickname:string);
begin
  (*
  fPlayer:= TPlayer.Create;
  fPlayer.name:= aNickname;
  fPlayer.location:= 'lobby';
  Players.Add(fPlayer);
  *)

  fNickname:= aNickname;
  fLoggedIn:= true;
end;

procedure TIWUserSession.Set_Player_location(location: string);
begin
  //fPlayer.location:= location;
end;

end.
