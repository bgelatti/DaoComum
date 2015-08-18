object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 260
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
  object Label1: TLabel
    Left = 124
    Top = 114
    Width = 14
    Height = 13
    Caption = 'Id:'
  end
  object Label2: TLabel
    Left = 107
    Top = 141
    Width = 31
    Height = 13
    Caption = 'Nome:'
  end
  object Edit1: TEdit
    Left = 152
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 88
    Top = 35
    Width = 97
    Height = 25
    Caption = 'Nome Tabela'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 224
    Top = 35
    Width = 105
    Height = 25
    Caption = 'Chave Primaria'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 152
    Top = 165
    Width = 97
    Height = 25
    Caption = 'Inserir'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Edit2: TEdit
    Left = 144
    Top = 111
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object Edit3: TEdit
    Left = 144
    Top = 138
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object Button4: TButton
    Left = 152
    Top = 196
    Width = 97
    Height = 25
    Caption = 'Alterar'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 152
    Top = 227
    Width = 97
    Height = 25
    Caption = 'Excluir'
    TabOrder = 7
    OnClick = Button5Click
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
    Left = 24
    Top = 56
  end
  object PostgreSQLUniProvider1: TPostgreSQLUniProvider
    Left = 384
    Top = 16
  end
end
