unit ServerController;

interface

uses
  SysUtils,
  Classes,
  IWServerControllerBase,
  IWBaseForm,
  HTTPApp,
  // For OnNewSession Event
  UserSessionUnit,
  IdThreadSafe,
  IWApplication,
  IWAppForm,
  IW.Browser.Browser,
  IW.HTTP.Request,
  IW.HTTP.Reply,
  Generics.Collections,
  MS_Types,
  SyncObjs;

type
  TIWServerController = class( TIWServerControllerBase )
    procedure IWServerControllerBaseNewSession( ASession : TIWApplication );
    procedure IWServerControllerBaseCloseSession(aSession: TIWApplication);

  private
    { Private declarations }

  public
    { Public declarations }
    //procedure IWLog(msg:string);
    procedure IWSaveScores(line:string);
  end;

function UserSession : TIWUserSession;
function IWServerController : TIWServerController;

const DebugMode : boolean = {$IFDEF DEBUG}True{$ELSE}False{$ENDIF};
const scores_fileName = 'Logs\scores.csv';
var Connected_user_count: TIdThreadSafeInteger;
	  IWLogsCS: TCriticalSection;

implementation

{$R *.dfm}

uses
  IWInit,
  IWGlobal;


function IWServerController : TIWServerController;
begin
  Result := TIWServerController( GServerController );
end;

function UserSession : TIWUserSession;
begin
  Result := TIWUserSession( WebApplication.Data );
end;

{ TIWServerController }

procedure TIWServerController.IWServerControllerBaseCloseSession( aSession: TIWApplication);
begin
  Connected_user_count.Decrement;
  if Connected_user_count.Value < 0 then
     Connected_user_count.Value:= 0;
end;

procedure TIWServerController.IWServerControllerBaseNewSession( ASession : TIWApplication );
begin
  Connected_user_count.Increment;
  ASession.Data := TIWUserSession.Create( nil, ASession );
end;

procedure TIWServerController.IWSaveScores(line:string);
begin
  try
    IWLogsCS.Acquire;
    try
      var logFile: textFile;
      try
        AssignFile(logFile,scores_fileName);
        if fileExists(scores_fileName)
          then Append(logFile)
        else Rewrite(logFile);
          WriteLn(logFile,line);

      finally
        CloseFile(logFile);
      end;

    finally
      IWLogsCS.Release;
    end;

  except
    on E:Exception do
  end;
end;

initialization

  CreateDir('Logs');
  TIWServerController.SetServerControllerClass;
  Connected_user_count:= TIdThreadSafeInteger.Create;
  Connected_user_count.Value:= 0;
  IWLogsCS:= TCriticalSection.Create;

finalization

  Connected_user_count.Free;
  IWLogsCS.Free;


end.
