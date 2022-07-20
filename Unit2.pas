unit Unit2;

interface

uses
  Classes,
  SysUtils,
  Vcl.Controls,
  ServerController,
  unit3,
  DateUtils,

  IWAppForm,
  IWApplication,
  IWColor,
  IWTypes,
  IWCompGradButton,
  IWVCLBaseControl,
  IWBaseControl,
  IWBaseHTMLControl,
  IWControl,
  IWCompLabel,
  IWBaseComponent,
  IWBaseHTMLComponent,
  IWBaseHTML40Component,
  IWCompExtCtrls,
  IWCompRectangle,
  IWCompGrids,
  IWBS4CustomControl,
  IWBS4InputCheckbox,
  IWCompEdit, IWCompListbox, IWHTMLControls;

const max_hints_per_row_col = 5;
const size_px  = 48;

type
  TIWForm_Game = class( TIWAppForm )
    IWGradButton1 : TIWGradButton;
    IWTimer1 : TIWTimer;
    IWGradButton2: TIWGradButton;
    IWGrid1: TIWGrid;
    IWRadioGroup1: TIWRadioGroup;
    IWLabel_mistakes: TIWLabel;
    IWLabel_time: TIWLabel;
    IWTimer_Ending: TIWTimer;
    IWLabel_online_users: TIWLabel;
    IWTimer2: TIWTimer;
    IWComboBox_difficulty: TIWComboBox;
    IWURL1: TIWURL;
    procedure IWTimer1AsyncTimer( Sender : TObject; EventParams : TStringList );
    procedure IWGradButton2AsyncClick(Sender: TObject;
      EventParams: TStringList);
    procedure IWRadioGroup1AsyncChange(Sender: TObject;
      EventParams: TStringList);
    procedure IWTimer_EndingAsyncTimer(Sender: TObject;
      EventParams: TStringList);
    procedure IWGradButton1AsyncClick(Sender: TObject;
      EventParams: TStringList);
    procedure IWAppFormDestroy(Sender: TObject);
    procedure IWTimer2AsyncTimer(Sender: TObject; EventParams: TStringList);
  public
    var desired_grid_size: integer;

    var grid            : array of array of boolean;
    var cells           : array of array of TIWGradButton;
    var hints_vertical  : array of TStringList;
    var hints_horizontal: array of TStringList;

    var game_status_over: boolean;
    var marking_nodes: boolean;
    var mistakes: integer;
    var number_of_inputs: integer;

    var start_timestamp: TDateTime;

    procedure Cell_click(Sender: TObject);
    procedure Generate_grid_buttons;
    procedure Generate_hint_labels;
    procedure Update_time_label;
    procedure Game_start;
    procedure Game_over;
    procedure Game_won;
    procedure Clean_up;
    procedure Generate_grid_values;
    function Check_game_won:boolean;
    function Check_mistakes:boolean;
    function validate_hints: boolean;
  end;

implementation

{$R *.dfm}

procedure TIWForm_Game.Generate_grid_values;
begin
  randomize;
  var grid_size := desired_grid_size - 1;

  for var i := 0 to grid_size do
  for var j := 0 to grid_size do
    grid[i,j]:= random(100)>45;
end;

procedure TIWForm_Game.Game_over;
begin
  game_status_over:= true;
  IWLabel_mistakes.Caption:= 'GAME OVER!';
  IWTimer1.Enabled := false;
  IWLabel_time.Caption:= 'Time remaining: 0:00';
end;

procedure TIWForm_Game.Game_won;
begin
  number_of_inputs:= -1;
  game_status_over:= true;

  IWLabel_mistakes.Caption:= 'GAME WON!';
  IWTimer1.Enabled := false;
  IWTimer_Ending.Enabled:= true;
end;

function TIWForm_Game.Check_game_won:boolean;
begin
  result:= false;
  var number_of_cells:= desired_grid_size * desired_grid_size;
  if number_of_inputs=number_of_cells then
    begin
      result:= true;
      Game_won;
    end;
end;

function TIWForm_Game.Check_mistakes:boolean;
begin
  result:= false;
  IWLabel_mistakes.Caption:= 'Mistakes: '+mistakes.ToString + ' / 5';
  if mistakes=5 then
    begin
      result:= true;
      Game_over;
    end;
end;

procedure TIWForm_Game.Cell_click(Sender: TObject);
begin
  if game_status_over then exit;

  try
    var tag:= (Sender as TIWGradButton).Tag;
    if tag<0 then
      begin
        WebApplication.ShowNotification('Invalid index',ntError);
        exit;
      end;

    //WebApplication.ShowNotification('Button #'+tag.ToString,ntSuccess);

    var row:= tag div desired_grid_size;
    var col:= tag mod desired_grid_size;

    var already_revealed:= cells[row,col].Style.ColorScheme <> csWhite;
    if already_revealed then exit;

    var competition_mode:= UserSession.difficulty=2;
    var node_color:= csGreen;
    if competition_mode then
      node_color:= csYellow;

    if marking_nodes then
      begin
        if grid[row,col]=true then
          cells[row,col].Style.ColorScheme:= node_color
        else
          begin
            cells[row,col].Style.ColorScheme:= csLightGray;
            cells[row,col].Caption:= 'X';
            inc(mistakes);
          end;
      end
    else
      begin
        if grid[row,col]=false then
          cells[row,col].Style.ColorScheme:= csLightGray
        else
          begin
            cells[row,col].Style.ColorScheme:= node_color;
            cells[row,col].Caption:= 'X';
            inc(mistakes);
          end;
      end;

    if Check_mistakes then exit;

    inc(number_of_inputs);
    if Check_game_won then exit;

  except
    on E: Exception do
      WebApplication.ShowNotification('[Cell_click] '+ E.Message,ntError);
  end;
end;

procedure TIWForm_Game.Generate_hint_labels;
begin
  var grid_size := desired_grid_size - 1;

  // vertical
  for var i := 0 to grid_size do
    IWGrid1.Cell[i+1,0].Text:= hints_vertical[i].CommaText;

  //horizontal
  for var j := 0 to grid_size do
    IWGrid1.Cell[0,j+1].Text:= hints_horizontal[j].CommaText;
end;

procedure TIWForm_Game.Generate_grid_buttons;
begin
  var grid_size := desired_grid_size - 1;
  var counter:= 0;
  for var i := 0 to grid_size do
  for var j := 0 to grid_size do
    begin
      var cell:= TIWGradButton.Create(self);
      cell.Parent:= self;
      cell.Width := size_px;
      cell.Height:= size_px;
      cell.Tag := counter;
      cell.name:= 'cell'+counter.ToString;
      cell.Caption:= '';
      cell.Style.ColorScheme:= csWhite;
      cell.OnClick:= Cell_click;

      IWGrid1.Cell[i+1,j+1].Control:= cell;
      cells[i,j]:= cell;
      inc(counter);
    end;

  WebApplication.ShowNotification('ready',ntLog);
end;

function TIWForm_Game.validate_hints: boolean;

  function Add_hint_to_array(vertical: boolean; slot:integer; value:integer): boolean;
  begin
    result:= true;

    if vertical then
      begin
        hints_vertical[slot].Add(value.ToString);
        if hints_vertical[slot].Count>max_hints_per_row_col then exit(false);
      end
    else
      begin
        hints_horizontal[slot].Add(value.ToString);
        if hints_horizontal[slot].Count>max_hints_per_row_col then exit(false);
      end;
  end;

begin
  var grid_size := desired_grid_size - 1;
  for var i := 0 to grid_size do
    hints_vertical[i]:= TStringList.Create;
  for var j := 0 to grid_size do
    hints_horizontal[j]:= TStringList.Create;

  for var i := 0 to grid_size do
    begin
      var positive_in_sequence:= 0;
      var last_was_positive:= false;

      for var j := 0 to grid_size do
        begin
          var this_cell_positive:= grid[i,j];

          if this_cell_positive then
            begin
              inc(positive_in_sequence);
              last_was_positive:= true;
            end
          else
            begin
              if last_was_positive then
                if not Add_hint_to_array(true,i,positive_in_sequence) then
                  exit(false);

              positive_in_sequence:= 0;
              last_was_positive:= false;
            end;
        end;

      if last_was_positive then
        if not Add_hint_to_array(true,i,positive_in_sequence) then
          exit(false);
      if hints_vertical[i].Count<1 then
         hints_vertical[i].Add('0');
    end;

  for var j := 0 to grid_size do
    begin
      var positive_in_sequence:= 0;
      var last_was_positive:= false;

      for var i := 0 to grid_size do
        begin
          var this_cell_positive:= grid[i,j];

          if this_cell_positive then
            begin
              inc(positive_in_sequence);
              last_was_positive:= true;
            end
          else
            begin
              if last_was_positive then
                if not Add_hint_to_array(false,j,positive_in_sequence) then
                  exit(false);

              positive_in_sequence:= 0;
              last_was_positive:= false;
            end;
        end;

      if last_was_positive then
        if not Add_hint_to_array(false,j,positive_in_sequence) then
          exit(false);
      if hints_horizontal[j].Count<1 then
         hints_horizontal[j].Add('0');
    end;

  result:= true;
end;

procedure TIWForm_Game.Game_start;
begin
  start_timestamp:= Now;
  Update_time_label;
  IWLabel_time.Visible:= true;
  Clean_up;

  const size_small  = 6;
  const size_medium = 8;
  const size_competition  = 10;
  const size_large  = 12;
  const size_huge   = 16;

  case UserSession.difficulty of
  0 : desired_grid_size:= size_small;
  1 : desired_grid_size:= size_medium;
  2 : desired_grid_size:= size_competition;
  3 : desired_grid_size:= size_large;
  4 : desired_grid_size:= size_huge;
  end;

  if DebugMode then
    desired_grid_size:= 4;

  SetLength(grid, desired_grid_size,desired_grid_size);
  SetLength(cells,desired_grid_size,desired_grid_size);
  SetLength(hints_vertical,   desired_grid_size);
  SetLength(hints_horizontal, desired_grid_size);

  var tries:= 0;
  repeat
    Generate_grid_values;
    inc(tries);
    if tries>100 then
      begin
        WebApplication.ShowNotification('Failed to validate grid, please try again',ntError);
        exit;
      end;

  until Validate_hints;

  const hint_rows_cols = 1;
  var grid_size := desired_grid_size - 1;
  IWGrid1.RowCount:=    hint_rows_cols + grid_size +1;
  IWGrid1.ColumnCount:= hint_rows_cols + grid_size +1;
  IWGrid1.Width:=      (hint_rows_cols + grid_size) * size_px;
  IWGrid1.Height:=     (hint_rows_cols + grid_size) * size_px;

  //WebApplication.ShowNotification('Screen width: '+IWGrid1.Width.ToString,ntLog);

  Generate_hint_labels;
  Generate_grid_buttons;

  IWRadioGroup1.ItemIndex:= 0;
  game_status_over:= false;
  marking_nodes:= true;
  mistakes:= 0;
  number_of_inputs:= 0;

  Check_mistakes;

  start_timestamp:= Now;
  IWTimer1.Enabled := true;
end;

procedure TIWForm_Game.IWGradButton2AsyncClick(Sender: TObject;
  EventParams: TStringList);
begin
  if IWComboBox_difficulty.ItemIndex=2 then
    begin
      WebApplication.ShowNotification('No active competitions at the moment',ntError);
      exit;
    end;

  UserSession.difficulty:= IWComboBox_difficulty.ItemIndex;
  Game_start;
end;

procedure TIWForm_Game.IWRadioGroup1AsyncChange(Sender: TObject;
  EventParams: TStringList);
begin
  marking_nodes:= IWRadioGroup1.ItemIndex=0;
end;

procedure TIWForm_Game.Update_time_label;
var seconds_buffer: string;
begin
  var elapsed_time:= secondsbetween(Now,start_timestamp);
  var remaining_time:= 600 - elapsed_time;
  IWTimer1.Tag:= remaining_time;

  var minutes_remain:= IWTimer1.tag div 60;
  var seconds_remain:= IWTimer1.tag mod 60;

  if seconds_remain<10 then
    seconds_buffer:= '0'
  else
    seconds_buffer:= '';

  IWLabel_time.Caption:=
    'Time remaining: ' +
    minutes_remain.ToString +
    ':' +
    seconds_buffer +
    seconds_remain.ToString;
end;

procedure TIWForm_Game.IWTimer1AsyncTimer( Sender : TObject; EventParams : TStringList );
begin
  // UserSession.Set_Player_location('game');

  IWTimer1.tag:= IWTimer1.tag - 1;

  if IWTimer1.tag<0 then
    begin
      Game_over;
      exit;
    end;

  Update_time_label;
end;

procedure TIWForm_Game.IWTimer2AsyncTimer(Sender: TObject;
  EventParams: TStringList);
begin
  IWLabel_online_users.Text := 'Online users: '+Connected_user_count.Value.ToString;
end;

procedure TIWForm_Game.IWTimer_EndingAsyncTimer(Sender: TObject;
  EventParams: TStringList);
begin
  IWTimer_Ending.Enabled:= false;

  var seconds_remaining:= IWTimer1.tag;
  UserSession.score:= (seconds_remaining * desired_grid_size) - (mistakes * 100);
  if UserSession.difficulty=3 then inc(UserSession.score,10000);

  if UserSession.score<1 then
     UserSession.score:= 1;

  TIWForm_Ending.Create(WebApplication).Show;
end;

procedure TIWForm_Game.Clean_up;
begin
  try
    for var i := 0 to high(cells) do
      begin
        for var j := 0 to high(cells) do
          if assigned(cells[i,j]) then
            cells[i,j].Free;
        if assigned(hints_vertical[i]) then
          hints_vertical[i].Free;
        if assigned(hints_horizontal[i]) then
          hints_horizontal[i].Free;
      end;

  except
  end;
end;

procedure TIWForm_Game.IWAppFormDestroy(Sender: TObject);
begin
  Clean_up;
end;

procedure TIWForm_Game.IWGradButton1AsyncClick(Sender: TObject;
  EventParams: TStringList);
begin
  Hide;
end;

end.
