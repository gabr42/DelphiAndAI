object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Clipboard monitor'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    Align = alClient
    Lines.Strings = (
      '')
    TabOrder = 0
    ExplicitLeft = 232
    ExplicitTop = 200
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 304
    Top = 224
  end
end
