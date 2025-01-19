object Form14: TForm14
  Left = 0
  Top = 0
  Caption = 'Form14'
  ClientHeight = 441
  ClientWidth = 677
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    677
    441)
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 42
    Height = 15
    Caption = 'Original'
  end
  object Label2: TLabel
    Left = 321
    Top = 11
    Width = 49
    Height = 15
    Caption = 'Encoded:'
  end
  object Label3: TLabel
    Left = 320
    Top = 61
    Width = 50
    Height = 15
    Caption = 'Decoded:'
  end
  object btnGemini: TButton
    Left = 208
    Top = 31
    Width = 97
    Height = 25
    Caption = 'Gemini'
    TabOrder = 0
    OnClick = btnGeminiClick
  end
  object inpText: TMemo
    Left = 8
    Top = 32
    Width = 185
    Height = 89
    Lines.Strings = (
      'Test: '#229#234#238#248#468
      'Delimiters: #42\x42'
      #55357#56832)
    TabOrder = 1
  end
  object outEncoded: TEdit
    Left = 320
    Top = 32
    Width = 349
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitWidth = 347
  end
  object outDecoded: TMemo
    Left = 320
    Top = 82
    Width = 349
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    ExplicitWidth = 347
  end
  object btnGeminiFixed: TButton
    Left = 208
    Top = 62
    Width = 97
    Height = 25
    Caption = 'Gemini (fixed)'
    TabOrder = 4
    OnClick = btnGeminiFixedClick
  end
  object btnClaude: TButton
    Left = 208
    Top = 93
    Width = 97
    Height = 25
    Caption = 'Claude'
    TabOrder = 5
    OnClick = btnClaudeClick
  end
  object btnClaudeFixed: TButton
    Left = 208
    Top = 124
    Width = 97
    Height = 25
    Caption = 'Claude (fixed)'
    TabOrder = 6
    OnClick = btnClaudeFixedClick
  end
  object btnCoPilot: TButton
    Left = 208
    Top = 155
    Width = 97
    Height = 25
    Caption = 'CoPilot'
    TabOrder = 7
    OnClick = btnCoPilotClick
  end
  object btnCoPilotFixed: TButton
    Left = 208
    Top = 186
    Width = 97
    Height = 25
    Caption = 'CoPilot (fixed)'
    TabOrder = 8
    OnClick = btnCoPilotFixedClick
  end
  object btnOpenAI: TButton
    Left = 208
    Top = 217
    Width = 97
    Height = 25
    Caption = 'OpenAI'
    TabOrder = 9
    OnClick = btnOpenAIClick
  end
  object btnOpenAIFixed: TButton
    Left = 208
    Top = 248
    Width = 97
    Height = 25
    Caption = 'OpenAI (fixed)'
    TabOrder = 10
    OnClick = btnOpenAIFixedClick
  end
end
