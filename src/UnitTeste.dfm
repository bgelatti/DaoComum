object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 234
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 152
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 104
    Top = 112
    Width = 97
    Height = 25
    Caption = 'Nome Tabela'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 248
    Top = 112
    Width = 105
    Height = 25
    Caption = 'Chave Primaria'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 104
    Top = 160
    Width = 97
    Height = 25
    Caption = 'Inserir'
    TabOrder = 3
    OnClick = Button3Click
  end
  object UniConnection1: TUniConnection
    ProviderName = 'PostgreSQL'
    Port = 5432
    Database = 'system'
    Username = 'sigar'
    Server = 'localhost'
    Connected = True
    Left = 32
    Top = 8
    EncryptedPassword = '98FF9AFF93FF8CFF96FF98FF9EFF8DFF'
  end
  object UniTransaction1: TUniTransaction
    DefaultConnection = UniConnection1
    Left = 32
    Top = 64
  end
  object PostgreSQLUniProvider1: TPostgreSQLUniProvider
    Left = 32
    Top = 120
  end
end
