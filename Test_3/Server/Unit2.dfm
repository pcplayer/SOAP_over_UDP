object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'SOAP over UDP Server'
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
    Left = 56
    Top = 88
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 56
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Open UDP'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 208
    Top = 0
    Width = 427
    Height = 299
    Align = alRight
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitLeft = 204
    ExplicitHeight = 298
  end
  object IdUDPServer1: TIdUDPServer
    Bindings = <>
    DefaultPort = 664
    OnUDPRead = IdUDPServer1UDPRead
    Left = 80
    Top = 176
  end
end
