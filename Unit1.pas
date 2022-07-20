unit Unit1;

interface

uses
  Classes,
  SysUtils,
  Vcl.Controls,
  ServerController,
  Unit2,

  IWAppForm,
  IWApplication,
  IWColor,
  IWTypes,
  IWVCLBaseControl,
  IWBaseControl,
  IWBaseHTMLControl,
  IWControl,
  IWCompGradButton,
  IWCompMemo,
  IWCompLabel,
  IWCompEdit,
  Vcl.Forms,
  IWVCLBaseContainer,
  IWContainer,
  IWHTMLContainer,
  IWHTML40Container,
  IWRegion,
  Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage,
  IWCompExtCtrls,
  IWCompButton,
  DiscordAPI, IWHTMLControls;

type
  TIWForm1 = class( TIWAppForm )
    IWGradButton2 : TIWGradButton;
    IWImage1: TIWImage;
    IWImage2: TIWImage;
    IWLabel1: TIWLabel;
    IWURL1: TIWURL;
    IWButton1: TIWButton;
    procedure IWGradButton2Click( Sender : TObject );
    procedure IWButton1AsyncClick(Sender: TObject; EventParams: TStringList);
    procedure IWAppFormCreate(Sender: TObject);
  public
    game_form: TIWForm_Game;
  end;

implementation

{$R *.dfm}


procedure TIWForm1.IWAppFormCreate(Sender: TObject);
begin
  if DebugMode then
    IWButton1.Visible:= true;
end;

procedure TIWForm1.IWButton1AsyncClick(Sender: TObject;
  EventParams: TStringList);
begin
  const emoji_trophy = ':trophy: ';
  var msg:= emoji_trophy +'NEW COMPETITION ENTRY'+ emoji_trophy +'Orange' +' Score: 1234';
  SendChannelBasicMessage(msg);
end;

procedure TIWForm1.IWGradButton2Click( Sender : TObject );
begin
  if game_form=nil then
    game_form:= TIWForm_Game.Create(WebApplication);
  game_form.Show;
end;

initialization

TIWForm1.SetAsMainForm;

end.
