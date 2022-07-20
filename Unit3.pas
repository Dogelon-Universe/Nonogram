unit Unit3;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  ServerController,
  DiscordAPI,
  Vcl.Imaging.jpeg,
  Vcl.Controls,
  IWAppForm,
  IWApplication,
  IWColor,
  IWTypes,
  IWVCLBaseControl,
  IWBaseControl,
  IWBaseHTMLControl,
  IWControl,
  IWCompExtCtrls,
  IWCompEdit,
  IWCompGradButton,
  IWCompLabel,
  IWHTMLControls,
  iniFiles;

type
  TIWForm_Ending = class( TIWAppForm )
    IWImage1 : TIWImage;
    IWGradButton1 : TIWGradButton;
    IWEdit_name : TIWEdit;
    IWGradButton_add_name: TIWGradButton;
    IWLabel1: TIWLabel;
    IWLabel_score: TIWLabel;
    IWLabel2: TIWLabel;
    IWURL1: TIWURL;
    procedure IWGradButton_add_nameAsyncClick( Sender : TObject; EventParams : TStringList );
    procedure IWGradButton1AsyncClick( Sender : TObject; EventParams : TStringList );
    procedure IWAppFormCreate(Sender: TObject);
  public
  end;

implementation

{$R *.dfm}

procedure TIWForm_Ending.IWAppFormCreate(Sender: TObject);
begin
  IWLabel_score.Caption:= 'Score: '+ UserSession.score.ToString;

  var competition_mode:= UserSession.difficulty=2;
  if competition_mode then
    IWGradButton_add_name.Caption:= 'Add your (MF discord) username to be eligible to win the NFT!';

  var username := UserSession.GetCookieValue( 'u' );
  if not username.IsEmpty then
    IWEdit_name.Text:= username;
end;

procedure TIWForm_Ending.IWGradButton1AsyncClick( Sender : TObject; EventParams : TStringList );
begin
  IWGradButton_add_name.Enabled:= true;
  Hide;
end;

function ReadINI(section,key: string; filename:string=''; default:string=''): string;
begin
  var default_ini_filename := GetCurrentDir +'\settings.ini';
  var appINI := TIniFile.Create(default_ini_filename);
  try
    result:= appINI.ReadString(section,key,default);
  finally
    appINI.Free;
  end;
end;

function UpdateINI(section,key: string; value:string; filename:string=''): boolean;
begin
  var default_ini_filename := GetCurrentDir +'\settings.ini';
  var appINI := TIniFile.Create(default_ini_filename);
  try
    appINI.WriteString(section,key,value);
    result:= true;
  finally
    appINI.Free;
  end;
end;

function Read_previous_hiscore(difficulty:integer): string;
begin
  result:= ReadINI('hi-scores',difficulty.ToString);
end;

function Save_hiscore(difficulty:integer; values:string): string;
begin
  UpdateINI('hi-scores',difficulty.ToString,values);
end;

function ExtractValue(const xline:string; const which:integer; AddQuotes:boolean=false):string;
begin
  result:= xline;
  if xline.IsEmpty      then exit;
  if (Pos(',',xline)=0) then exit;

  result:= '';

  var work:= TStringList.Create;
  work.Delimiter:= ',';
  work.StrictDelimiter:= true;
  work.CommaText:= xline;

  try
    try
      if (which<0) OR (which>work.Count-1) then
        result:='BAD_INDEX'
      else
        result:= work[which];
    except
      on Exception do
        result:='ERROR';
    end;

  finally
    work.Free;
  end;
end;

procedure TIWForm_Ending.IWGradButton_add_nameAsyncClick( Sender : TObject; EventParams : TStringList );
var diff, previous_best_user,previous_best_score: string;
    new_hiscore: boolean;

  function Pick_random_sentence:string;
  begin
    var sentences:= TStringlist.Create;
    try
      sentences.Add('You have finished the game demo!');
      sentences.Add('You must be butter because you''re on a roll!');
      sentences.Add('Is it just me or is it getting warm in here?');
      sentences.Add('It is rumored that Methuselon Mars had to walk uphill to school... BOTH WAYS!');
      sentences.Add('I once knew a man with a wooden leg but a real foot.');
      sentences.Add('Your future is bright, better bring sunglasses!');
      sentences.Add('Have you done this before?');
      sentences.Add('Is this thing on?');
      sentences.Add('Come on! Those are lunar numbers!');
      sentences.Add('A true Martian strives to do better everyday!');
      sentences.Add('The Annihilators have trouble spelling it too, don''t worry.');
      sentences.Add('A true Dogelon Warrior in the making.');
      sentences.Add('Launch sequence initiated, takeoff imminent!');
      sentences.Add('Coming in hot!');
      sentences.Add('To live long and prosper is the goal.');
      sentences.Add('Try it with your eyes open next time.');
      sentences.Add('Now that is a Martian score for sure!');
      sentences.Add('Look who just pulled up in a Lambo!');
      sentences.Add('Somebody''s been spamming that new game button.');
      sentences.Add('The name''s Mars... Rufus Mars.');
      sentences.Add('Has anybody seen my spaceship?');
      sentences.Add('Space travel is fun and all but have you ever had tacos?');
      sentences.Add('Don''t forget to pack clean underwear, space travel can be nerve-wracking.');
      sentences.Add('Do you think young Dogelon walks to school or carries a lunch?');
      sentences.Add('The Ruf, the Ruf, the Rufus on fire!');
      sentences.Add('I wonder what Xorn''s belly tavern has on draft?');
      sentences.Add('A long time ago in a galaxy far, far...wait a minute. Wrong saga.');
      sentences.Add('Rufus if you ever travel to the future and back could you bring me a sports almanac?');
      sentences.Add('Best not to tailgate other rocketships, you will inevitably become exhausted.');
      sentences.Add('The Annihilators don''t stand a chance with you on our side!');

      result:= sentences[random(sentences.Count-1)];

    finally
      sentences.Free;
    end;
  end;

begin
  var username := IWEdit_name.Text;
  if trim(username)='' then
    begin
      WebApplication.ShowNotification('Username cannot be empty',ntError);
      exit;
    end;

  username:= stringreplace(username,' ','',[rfReplaceAll, rfIgnoreCase]);
  username:= stringreplace(username,',','',[rfReplaceAll, rfIgnoreCase]);

  UserSession.SetCookie( 'u', username, 999 );

  IWGradButton_add_name.Enabled:= false;

  const comma: string = ',';
  var this_run_values:=
    formatdatetime('yyyymmdd_hhnn',Now) +comma+
    UserSession.difficulty.toString +comma+
    username +comma+
    UserSession.score.toString;

  new_hiscore:= false;
  var previous_hi_score:= Read_previous_hiscore(UserSession.difficulty);
  if not previous_hi_score.IsEmpty then
    begin
      previous_best_user:=  ExtractValue(previous_hi_score,2);
      previous_best_score:= ExtractValue(previous_hi_score,3);

      new_hiscore:= UserSession.score > previous_best_score.ToInteger;
      if new_hiscore then
        Save_hiscore(UserSession.difficulty, this_run_values);
    end
  else
    Save_hiscore(UserSession.difficulty, this_run_values);

  case UserSession.difficulty of
  0 : diff:= ' (easy difficulty)';
  1 : diff:= ' (medium difficulty)';
  3 : diff:= ' (hard difficulty)';
  4 : diff:= ' (insane difficulty)';
  end;

  var msg:= '';

  const emoji_fire = ' :fire: ';
  const emoji_trophy = ' :trophy: ';

  if new_hiscore then
    msg:= emoji_fire +username+' BROKE THE PREVIOUS RECORD '+previous_best_score+' made by '+previous_best_user+'!'+
      ' Score: '+UserSession.score.ToString + diff
  else
    msg:= username +'! '+Pick_random_sentence +
      ' Score: '+UserSession.score.ToString + diff;

  var competition_mode:= UserSession.difficulty=2;
  if competition_mode then
    msg:= emoji_trophy +'NEW COMPETITION ENTRY'+ emoji_trophy +username +', score: '+UserSession.score.ToString;

  if DebugMode then
    SendChannelBasicMessage(msg)
  else
    SendChannelBasicMessage(msg,true);

  IWServerController.IWSaveScores( this_run_values );

  WebApplication.ShowNotification('Score sent to discord',ntSuccess);
end;

end.
