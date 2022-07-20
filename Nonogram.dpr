program Nonogram;

uses
  FastMM4,
  IWRtlFix,
  IWJclStackTrace,
  IWJclDebug,
  IWStart,
  Unit1 in 'Unit1.pas' {IWForm1: TIWAppForm},
  ServerController in 'ServerController.pas' {IWServerController: TIWServerControllerBase},
  UserSessionUnit in 'UserSessionUnit.pas' {IWUserSession: TIWUserSessionBase},
  MS_Types in 'MS_Types.pas',
  Unit2 in 'Unit2.pas' {IWForm_Game: TIWAppForm},
  Unit3 in 'Unit3.pas' {IWForm_Ending: TIWAppForm},
  DiscordAPI in 'DiscordAPI.pas';

{$R *.res}

begin
  TIWStart.Execute(True);
end.
