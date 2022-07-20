unit DiscordAPI;
interface

uses
  System.Classes,
  System.SysUtils,
  System.NetConsts,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Net.Mime,
  Vcl.Graphics,
  iniFiles;

type
  TEmbedField = class
    Name: string;       //name of the field
    Value: string;	    //value of the field
    Inline_: boolean;   //whether or not this field should display inline
    function ToString: string; override;
  end;

  TEmbedFooter = class
    Text:string;              //footer text
    Icon_url: string;         //url of footer icon (only supports http(s) and attachments)
    Poxy_icon_url: string;   //	a proxied url of footer icon
    function ToString: string; override;
  end;

  TEmbedMedia = class
    Url:string;         //source url of media (only supports http(s) and attachments)
    Proxy_url: string;  //a proxied url of the media
    Height: Integer;    //height of media
    Width:	Integer;    //width of media
    Attachment: TStream;
    FAttachmentName: string;
    function ToString: string; override;
    Destructor  Destroy; override;
    procedure SetAttachmentName(const Value: string);
    property AttachmentName: string read FAttachmentName write SetAttachmentName;
  end;

  TEmbedProvider = class
    Name: string;   //name of provider
    Url: string;    //url of provider
    function ToString: string; override;
  end;

  TEmbedAuthor = class
    Name: string;           //name of author
    Url: string;            //url of author
    Icon_url:string;        //url of author icon (only supports http(s) and attachments)
    Proxy_icon_url: string; //	a proxied url of author icon
    function ToString: string; override;
  end;

  TDiscordEmbed = class
    Title: string;                  //title of embed
    //Type: string;                 //type of embed (always "rich" for webhook embeds)
    Description: string;            //description of embed
    Timestamp: TDateTime;           //ISO8601 timestamp	timestamp of embed content
    Color: string;                  //color code of the embed
    Url: string;                    //url of embed
    Fields: array of TEmbedField;   //Up to 25 field objects
    Footer: TEmbedFooter;         //embed footer object	footer information
    Image: TEmbedMedia;           //embed image object	image information
    Thumbnail: TEmbedMedia;       //embed thumbnail object	thumbnail information
    Video: TEmbedMedia;           //embed video object	video information
    Provider: TEmbedProvider;     //embed provider object	provider information
    Author: TEmbedAuthor;         //embed author object	author information
    function AddNewField:TEmbedField;overload;
    function AddNewField(aName, aValue: string; inline_: boolean = False):TEmbedField;overload;
    function SetImageStream(ImageStream: TStream; AttachmentName:String = 'image.png'):TEmbedMedia;
    function ToString: string; override;
    Destructor  Destroy; override;
  end;

  TDiscordMessage = class
    Username: string;
    Avatar_url: string;
    Content: string;
    Embeds: TDiscordEmbed;
    function ToString: string; override;
    function Send: string;
    Destructor  Destroy; override;
  end;


function SendChannelBasicMessage(msg: string; live_call:boolean=false): string;

const
  discord_prefix = 'https://discord.com/api/webhooks/';
var
  discord_webhook_test,discord_webhook_live: string;

implementation


function MakePair(Name, Value: string; isObject: boolean = False): string;
begin
  if (Value = '') or (Value = '0') then
  begin
    Result := '';
    Exit;
  end;

  var _Value: string;
  var ValueStringList := TStringlist.Create;
  try
    ValueStringList.Text := Value;

    if ValueStringList.Count = 1 then
      _Value := Value
    else
      for var I := 0 to ValueStringList.Count -1  do
        begin
         _Value := _Value + ValueStringList.Strings[I]+'\n'
        end;

  finally
   ValueStringList.Free;
  end;

  if not isObject then
     _Value := '"' + _Value +'"';
  Result := '"'+ Name +'":' + _Value + ',';
end;

function MakeObject(ObjStr: string): string;
begin
  if Length(ObjStr) < 1 then exit;

  if ObjStr[ObjStr.Length] = ',' then
    begin
      ObjStr := Copy(ObjStr, 0, ObjStr.Length -1 );
    end;
  Result := '{'+ ObjStr + '}';
end;


function Send_attachtment_to_discord_webhook(URL, JSON, AttachmentName: string; ChartStream: TStream): string;
begin
  var HTTP:= THTTPClient.Create;
  var Data:= TMultipartFormData.Create();
  try
    try
      HTTP.ContentType := 'multipart/form-data';
      data.AddStream(AttachmentName,ChartStream,AttachmentName);
      data.AddField('payload_json', JSON);

      var Response := HTTP.Post(URL,data);

      if (Response.StatusCode < 300) and (Response.StatusCode >= 200)  then
        Result:= 'Successful: ' + Response.StatusCode.ToString
      else
        Result:= 'Error: '+Response.StatusCode.ToString;

    except
      Result:= 'Error: Send_message_to_discord_webhook ';
    end;

  finally
    Data.Free;
    HTTP.Free;
  end;
end;

function Send_message_to_discord_webhook2(URL, JSON: string): string;
begin
  var HTTP:= THTTPClient.Create;
  var ReqJson := TStringStream.Create(JSON, TEncoding.UTF8);
  try
    try
      HTTP.ContentType := 'application/json;charset=UTF-8';
      var Response := HTTP.Post(URL,ReqJson);

      if (Response.StatusCode < 300) and (Response.StatusCode >= 200)  then
        Result:= 'Successful: ' + Response.StatusCode.ToString
      else
        Result:= 'Error: '+Response.StatusCode.ToString;

    except
      Result:= 'Error: Send_message_to_discord_webhook ';
    end;

  finally
    ReqJson.Free;
    HTTP.Free;
  end;
end;


function Send_message_to_discord_webhook(URL, JSON: string): string;
begin
  var HTTP:= THTTPClient.Create;
  var ReqJson := TStringStream.Create(JSON, TEncoding.UTF8);
  try
    try
      HTTP.ContentType := 'application/json;charset=UTF-8';
      var Response := HTTP.Post(URL,ReqJson);

      if (Response.StatusCode < 300) and (Response.StatusCode >= 200)  then
        Result:= 'Successful: ' + Response.StatusCode.ToString
      else
        Result:= 'Error: '+Response.StatusCode.ToString;

    except
      Result:= 'Error: Send_message_to_discord_webhook ';
    end;

  finally
    ReqJson.Free;
    HTTP.Free;
  end;
end;

function SendChannelBasicMessage(msg: string; live_call:boolean=false): string;
begin
  var destination:= discord_prefix + discord_webhook_test;
  if live_call then
    destination:= discord_prefix + discord_webhook_live;

  Result := Send_message_to_discord_webhook(destination, '{"content":"'+ msg +'"}');
end;

{ TEmbedField }

function TEmbedField.ToString: string;
begin
  Result := MakePair('name', self.Name);
  Result := Result + MakePair('value', self.Value);
  if self.Inline_ then
    Result := Result + MakePair('inline', 'true', true);
  Result := MakeObject(Result);
end;

{ TDiscordMessage }

destructor TDiscordMessage.Destroy;
begin
  try
    self.Embeds.Free;
  finally
  end;
  inherited;
end;

function TDiscordMessage.Send: string;
begin
  var WebHookURL:= discord_prefix + discord_webhook_live;

  if self.Embeds.Image = nil then
    Result := Send_message_to_discord_webhook(WebHookURL, Self.ToString)
  else Result := Send_attachtment_to_discord_webhook(webhookurl, self.ToString, self.Embeds.Image.AttachmentName, self.Embeds.Image.Attachment);

end;

function TDiscordMessage.ToString: string;
begin

  Result := MakePair('username', self.Username);
  Result := Result + MakePair('avatar_url', self.Avatar_url);
  Result := Result + MakePair('content', Self.Content);
  Result := Result + '"embeds":['+ self.Embeds.ToString +']';

  Result := MakeObject(Result);
end;

{ TDiscordEmbeds }

function TDiscordEmbed.AddNewField: TEmbedField;
begin
   SetLength(self.Fields, Length(self.Fields) + 1);
   result :=  TEmbedField.Create;
   Self.Fields[Length(self.Fields) - 1] := Result;
end;

function TDiscordEmbed.AddNewField(aName, aValue: string; inline_: boolean = False): TEmbedField;
begin
  Result := Self.AddNewField;
  Result.Name := aName;
  Result.Value := aValue;
  Result.Inline_ := inline_;
end;

function TDiscordEmbed.SetImageStream(ImageStream: TStream;
  AttachmentName: String): TEmbedMedia;
begin
   Self.Image := TEmbedMedia.Create;
   Self.Image.Attachment := ImageStream;
   Self.Image.AttachmentName := AttachmentName;
   Result := Self.Image;
end;

destructor TDiscordEmbed.Destroy;
begin
  try
   for var EmbedField  in Self.Fields do
    begin
      try
        if assigned(embedfield) then
          EmbedField.Free;
      finally

      end;
    end;

    try
    if Assigned(self.Author) then self.Author.Free;
    finally

    end;

    try
    if Assigned(self.Image) then self.Image.Free;
    finally

    end;

    try
    if Assigned(self.Thumbnail) then self.Thumbnail.Free;
    finally

    end;

  finally

  end;

  inherited;
end;


function TDiscordEmbed.ToString: string;
begin
  if Assigned(Self.Author) then
    Result := MakePair('author',self.Author.ToString, True)
    else Result := '';

  Result := Result + MakePair('title', self.Title);
  Result := Result + MakePair('description',self.Description);
  Result := Result + MakePair('color',self.Color);

  if Assigned(Self.Image) then
    Result := Result + MakePair('image',self.Image.ToString, True);

  if Assigned(Self.Thumbnail) then
    Result := Result + MakePair('thumbnail',self.Thumbnail.ToString, True);

  if length(self.Fields) > 0 then
  begin
    Result := Result + '"fields":[';
    for var EmbedField  in Self.Fields do
    begin
      Result := Result +  EmbedField.ToString + ',';
    end;
      Result := Copy(Result, 0, Result.Length -1 );
    Result := Result + ']'
  end;

  Result := MakeObject(Result);
end;

{ TEmbedAuthor }

function TEmbedAuthor.ToString: string;
begin
  Result := MakePair('name',self.Name);
  Result := Result + MakePair('url', self.Url);
  Result := Result + MakePair('icon_url',self.Icon_url);
  Result := Result + MakePair('proxy_icon_url',self.Proxy_icon_url);

  Result := MakeObject(Result);
end;

{ TEmbedFooter }

function TEmbedFooter.ToString: string;
begin

end;

{ TEmbedProvider }

function TEmbedProvider.ToString: string;
begin

end;

{ TEmbedMedia }

destructor TEmbedMedia.Destroy;
begin
  try
    if Attachment <> nil then
    begin
      Attachment.Free;
    end;
  finally

  end;

  inherited;
end;

procedure TEmbedMedia.SetAttachmentName(const Value: string);
begin
  FAttachmentName := Value;
  Url := 'attachment://' + Value;
end;

function TEmbedMedia.ToString: string;
begin
  Result := MakePair('url', self.Url);
  Result := Result + MakePair('proxy_url', self.Proxy_url);
  Result := Result + MakePair('width', self.Width.ToString);
  Result := Result + MakePair('height', self.Height.ToString);

  Result := MakeObject(Result);
end;

initialization

  var default_ini_filename := GetCurrentDir +'\settings.ini';
  var appINI := TIniFile.Create(default_ini_filename);
  try
    discord_webhook_test := appINI.ReadString('discord_webhooks','test','');
    discord_webhook_live := appINI.ReadString('discord_webhooks','live','');
  finally
    appINI.Free;
  end;

end.


