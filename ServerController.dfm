object IWServerController: TIWServerController
  OldCreateOrder = False
  AppName = 'Nonogram'
  Description = 'Dogelon Nonogram'
  DisplayName = 'Dogelon Nonogram'
  Port = 80
  Version = '15.2.27'
  SecurityOptions.CheckSameIP = True
  SecurityOptions.CheckSameUA = False
  SessionOptions.SessionTimeout = 10
  SessionOptions.RestartExpiredSession = True
  OnCloseSession = IWServerControllerBaseCloseSession
  OnNewSession = IWServerControllerBaseNewSession
  Height = 310
  Width = 342
end
