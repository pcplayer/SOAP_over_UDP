object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Soap over UDP Client'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 96
    Top = 104
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 96
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '100'
  end
  object Button1: TButton
    Left = 96
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Go'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 32
    Top = 25
    Width = 75
    Height = 25
    Caption = 'Open UDP'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 336
    Top = 0
    Width = 299
    Height = 299
    Align = alRight
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 3
    ExplicitLeft = 332
    ExplicitHeight = 298
  end
  object Button3: TButton
    Left = 96
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Go2'
    TabOrder = 4
    OnClick = Button3Click
  end
  object IdUDPServer1: TIdUDPServer
    Bindings = <>
    DefaultPort = 654
    OnUDPRead = IdUDPServer1UDPRead
    Left = 424
    Top = 192
  end
end
