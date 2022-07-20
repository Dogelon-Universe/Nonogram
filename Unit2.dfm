object IWForm_Game: TIWForm_Game
  Left = 0
  Top = 0
  Width = 535
  Height = 758
  RenderInvisibleControls = True
  AllowPageAccess = True
  ConnectionMode = cmAny
  ExtraHeader.Strings = (
    
      '<style>'#13#10'.switch {'#13#10'  font-size: 1rem;'#13#10'  position: relative;'#13#10'}' +
      #13#10'.switch input {'#13#10'  position: absolute;'#13#10'  height: 1px;'#13#10'  widt' +
      'h: 1px;'#13#10'  background: none;'#13#10'  border: 0;'#13#10'  clip: rect(0 0 0 0' +
      ');'#13#10'  clip-path: inset(50%);'#13#10'  overflow: hidden;'#13#10'  padding: 0;' +
      #13#10'}'#13#10'.switch input + label {'#13#10'  position: relative;'#13#10'  min-width' +
      ': calc(calc(2.375rem * .8) * 2);'#13#10'  border-radius: calc(2.375rem' +
      ' * .8);'#13#10'  height: calc(2.375rem * .8);'#13#10'  line-height: calc(2.3' +
      '75rem * .8);'#13#10'  display: inline-block;'#13#10'  cursor: pointer;'#13#10'  ou' +
      'tline: none;'#13#10'  user-select: none;'#13#10'  vertical-align: middle;'#13#10' ' +
      ' text-indent: calc(calc(calc(2.375rem * .8) * 2) + .5rem);'#13#10'}'#13#10'.' +
      'switch input + label::before,'#13#10'.switch input + label::after {'#13#10' ' +
      ' content: '#39#39';'#13#10'  position: absolute;'#13#10'  top: 0;'#13#10'  left: 0;'#13#10'  w' +
      'idth: calc(calc(2.375rem * .8) * 2);'#13#10'  bottom: 0;'#13#10'  display: b' +
      'lock;'#13#10'}'#13#10'.switch input + label::before {'#13#10'  right: 0;'#13#10'  backgr' +
      'ound-color: #dee2e6;'#13#10'  border-radius: calc(2.375rem * .8);'#13#10'  t' +
      'ransition: 0.2s all;'#13#10'}'#13#10'.switch input + label::after {'#13#10'  top: ' +
      '2px;'#13#10'  left: 2px;'#13#10'  width: calc(calc(2.375rem * .8) - calc(2px' +
      ' * 2));'#13#10'  height: calc(calc(2.375rem * .8) - calc(2px * 2));'#13#10' ' +
      ' border-radius: 50%;'#13#10'  background-color: white;'#13#10'  transition: ' +
      '0.2s all;'#13#10'}'#13#10'.switch input:checked + label::before {'#13#10'  backgro' +
      'und-color: #08d;'#13#10'}'#13#10'.switch input:checked + label::after {'#13#10'  m' +
      'argin-left: calc(2.375rem * .8);'#13#10'}'#13#10'.switch input:focus + label' +
      '::before {'#13#10'  outline: none;'#13#10'  box-shadow: 0 0 0 0.2rem rgba(0,' +
      ' 136, 221, 0.25);'#13#10'}'#13#10'.switch input:disabled + label {'#13#10'  color:' +
      ' #868e96;'#13#10'  cursor: not-allowed;'#13#10'}'#13#10'.switch input:disabled + l' +
      'abel::before {'#13#10'  background-color: #e9ecef;'#13#10'}'#13#10'.switch.switch-' +
      'sm {'#13#10'  font-size: 0.875rem;'#13#10'}'#13#10'.switch.switch-sm input + label' +
      ' {'#13#10'  min-width: calc(calc(1.9375rem * .8) * 2);'#13#10'  height: calc' +
      '(1.9375rem * .8);'#13#10'  line-height: calc(1.9375rem * .8);'#13#10'  text-' +
      'indent: calc(calc(calc(1.9375rem * .8) * 2) + .5rem);'#13#10'}'#13#10'.switc' +
      'h.switch-sm input + label::before {'#13#10'  width: calc(calc(1.9375re' +
      'm * .8) * 2);'#13#10'}'#13#10'.switch.switch-sm input + label::after {'#13#10'  wi' +
      'dth: calc(calc(1.9375rem * .8) - calc(2px * 2));'#13#10'  height: calc' +
      '(calc(1.9375rem * .8) - calc(2px * 2));'#13#10'}'#13#10'.switch.switch-sm in' +
      'put:checked + label::after {'#13#10'  margin-left: calc(1.9375rem * .8' +
      ');'#13#10'}'#13#10'.switch.switch-lg {'#13#10'  font-size: 1.25rem;'#13#10'}'#13#10'.switch.sw' +
      'itch-lg input + label {'#13#10'  min-width: calc(calc(3rem * .8) * 2);' +
      #13#10'  height: calc(3rem * .8);'#13#10'  line-height: calc(3rem * .8);'#13#10' ' +
      ' text-indent: calc(calc(calc(3rem * .8) * 2) + .5rem);'#13#10'}'#13#10'.swit' +
      'ch.switch-lg input + label::before {'#13#10'  width: calc(calc(3rem * ' +
      '.8) * 2);'#13#10'}'#13#10'.switch.switch-lg input + label::after {'#13#10'  width:' +
      ' calc(calc(3rem * .8) - calc(2px * 2));'#13#10'  height: calc(calc(3re' +
      'm * .8) - calc(2px * 2));'#13#10'}'#13#10'.switch.switch-lg input:checked + ' +
      'label::after {'#13#10'  margin-left: calc(3rem * .8);'#13#10'}'#13#10'.switch + .s' +
      'witch {'#13#10'  margin-left: 1rem;'#13#10'}'#13#10'</style>')
  OnDestroy = IWAppFormDestroy
  Background.Fixed = False
  HandleTabs = False
  LeftToRight = True
  LockUntilLoaded = False
  LockOnSubmit = False
  ShowHint = True
  DesignLeft = 2
  DesignTop = 2
  object IWGradButton1: TIWGradButton
    Left = 16
    Top = 16
    Width = 140
    Height = 36
    Style.Border.Color = 1051800
    Style.Border.Width = 1
    Style.BorderDisabled.Color = clWebGRAY
    Style.BorderDisabled.Width = 1
    Style.Button.Alignment = taCenter
    Style.Button.Font.Color = clWebWHITE
    Style.Button.Font.FontFamily = 'Arial, Sans-Serif, Verdana'
    Style.Button.Font.Size = 10
    Style.Button.Font.Style = []
    Style.Button.FromColor = 2366701
    Style.Button.ToColor = 1512362
    Style.ButtonHover.Alignment = taCenter
    Style.ButtonHover.Font.Color = clWebWHITE
    Style.ButtonHover.Font.FontFamily = 'Arial, Sans-Serif, Verdana'
    Style.ButtonHover.Font.Size = 10
    Style.ButtonHover.Font.Style = []
    Style.ButtonHover.FromColor = 1775049
    Style.ButtonHover.ToColor = 1380769
    Style.ButtonActive.Alignment = taCenter
    Style.ButtonActive.Font.Color = clWebWHITE
    Style.ButtonActive.Font.FontFamily = 'Arial, Sans-Serif, Verdana'
    Style.ButtonActive.Font.Size = 10
    Style.ButtonActive.Font.Style = []
    Style.ButtonActive.FromColor = 1512362
    Style.ButtonActive.ToColor = 2366701
    Style.ButtonDisabled.Alignment = taCenter
    Style.ButtonDisabled.Font.Color = clWebWHITE
    Style.ButtonDisabled.Font.FontFamily = 'Arial, Sans-Serif, Verdana'
    Style.ButtonDisabled.Font.Size = 10
    Style.ButtonDisabled.Font.Style = []
    Style.ButtonDisabled.FromColor = clWebLIGHTGRAY
    Style.ButtonDisabled.ToColor = 7368816
    Style.ColorScheme = csRed
    Style.BoxShadow.ColorTransparency = 0.500000000000000000
    Style.TextShadow.ColorTransparency = 0.500000000000000000
    Caption = 'Back'
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWGradButton1'
    OnAsyncClick = IWGradButton1AsyncClick
  end
  object IWGradButton2: TIWGradButton
    Left = 168
    Top = 16
    Width = 140
    Height = 36
    Style.Border.Color = 1051800
    Style.Border.Width = 1
    Style.BorderDisabled.Color = clWebGRAY
    Style.BorderDisabled.Width = 1
    Style.Button.Alignment = taCenter
    Style.Button.Font.Color = clWebWHITE
    Style.Button.Font.FontFamily = 'Arial, Sans-Serif, Verdana'
    Style.Button.Font.Size = 10
    Style.Button.Font.Style = []
    Style.Button.FromColor = 2366701
    Style.Button.ToColor = 1512362
    Style.ButtonHover.Alignment = taCenter
    Style.ButtonHover.Font.Color = clWebWHITE
    Style.ButtonHover.Font.FontFamily = 'Arial, Sans-Serif, Verdana'
    Style.ButtonHover.Font.Size = 10
    Style.ButtonHover.Font.Style = []
    Style.ButtonHover.FromColor = 1775049
    Style.ButtonHover.ToColor = 1380769
    Style.ButtonActive.Alignment = taCenter
    Style.ButtonActive.Font.Color = clWebWHITE
    Style.ButtonActive.Font.FontFamily = 'Arial, Sans-Serif, Verdana'
    Style.ButtonActive.Font.Size = 10
    Style.ButtonActive.Font.Style = []
    Style.ButtonActive.FromColor = 1512362
    Style.ButtonActive.ToColor = 2366701
    Style.ButtonDisabled.Alignment = taCenter
    Style.ButtonDisabled.Font.Color = clWebWHITE
    Style.ButtonDisabled.Font.FontFamily = 'Arial, Sans-Serif, Verdana'
    Style.ButtonDisabled.Font.Size = 10
    Style.ButtonDisabled.Font.Style = []
    Style.ButtonDisabled.FromColor = clWebLIGHTGRAY
    Style.ButtonDisabled.ToColor = 7368816
    Style.ColorScheme = csRed
    Style.BoxShadow.ColorTransparency = 0.500000000000000000
    Style.TextShadow.ColorTransparency = 0.500000000000000000
    Caption = 'New game'
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWGradButton2'
    OnAsyncClick = IWGradButton2AsyncClick
  end
  object IWGrid1: TIWGrid
    Left = 16
    Top = 240
    Width = 500
    Height = 500
    BorderColors.Color = clNone
    BorderColors.Light = clNone
    BorderColors.Dark = clNone
    BGColor = clNone
    BorderSize = 1
    BorderStyle = tfDefault
    Caption = 
      'Turn on the Baryonic HDM teleport! Fill all squares with either ' +
      'node/empty'
    CellPadding = 5
    CellSpacing = 0
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FrameBuffer = 40
    Lines = tlAll
    UseFrame = False
    UseSize = True
    HeaderRowCount = 0
    FriendlyName = 'IWGrid1'
    ColumnCount = 1
    RowCount = 1
    ShowEmptyCells = False
    ShowInvisibleRows = True
    ScrollToCurrentRow = False
  end
  object IWRadioGroup1: TIWRadioGroup
    Left = 16
    Top = 104
    Width = 185
    Height = 41
    SubmitOnAsyncEvent = True
    RawText = False
    Editable = True
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWRadioGroup1'
    ItemIndex = 0
    Items.Strings = (
      'Marking nodes'
      'Marking empty')
    Layout = glVertical
    OnAsyncChange = IWRadioGroup1AsyncChange
  end
  object IWLabel_mistakes: TIWLabel
    Left = 16
    Top = 159
    Width = 233
    Height = 16
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    HasTabOrder = False
    AutoSize = False
    FriendlyName = 'IWLabel_mistakes'
    Caption = 'Mistakes: 0'
  end
  object IWLabel_time: TIWLabel
    Left = 16
    Top = 191
    Width = 259
    Height = 25
    Font.Color = clNone
    Font.Size = 16
    Font.Style = [fsBold]
    HasTabOrder = False
    AutoSize = False
    FriendlyName = 'IWLabel_mistakes'
    Caption = 'Time remaining: 10:00'
  end
  object IWLabel_online_users: TIWLabel
    Left = 16
    Top = 64
    Width = 140
    Height = 16
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    HasTabOrder = False
    AutoSize = False
    FriendlyName = 'IWLabel_online_users'
    Caption = 'Online users: ?'
  end
  object IWComboBox_difficulty: TIWComboBox
    Left = 320
    Top = 16
    Width = 196
    Height = 21
    StyleRenderOptions.RenderBorder = False
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'Easy 6x6'
      'Medium 8x8'
      'NFT Competition 10x10'
      'Hard 12x12'
      'Insane 16x16')
    FriendlyName = 'IWComboBox_difficulty'
    NoSelectionText = '-- No Selection --'
  end
  object IWURL1: TIWURL
    Left = 320
    Top = 43
    Width = 196
    Height = 17
    Hint = 'https://www.google.com/search?q=how+to+play+nonogram'
    Alignment = taLeftJustify
    Color = clNone
    Font.Color = clNone
    Font.Size = 10
    Font.Style = [fsUnderline]
    HasTabOrder = True
    TargetOptions.AddressBar = False
    TerminateApp = False
    URL = 'https://www.google.com/search?q=how+to+play+nonogram'
    UseTarget = False
    FriendlyName = 'IWURL1'
    TabOrder = -1
    RawText = False
    Caption = 'Help! How do you play this?!'
  end
  object IWTimer1: TIWTimer
    Tag = 600
    Enabled = False
    Interval = 1000
    ShowAsyncLock = False
    OnAsyncTimer = IWTimer1AsyncTimer
    Left = 392
    Top = 88
  end
  object IWTimer_Ending: TIWTimer
    Enabled = False
    Interval = 3000
    ShowAsyncLock = False
    OnAsyncTimer = IWTimer_EndingAsyncTimer
    Left = 464
    Top = 88
  end
  object IWTimer2: TIWTimer
    Enabled = True
    Interval = 1000
    ShowAsyncLock = False
    OnAsyncTimer = IWTimer2AsyncTimer
    Left = 328
    Top = 88
  end
end
