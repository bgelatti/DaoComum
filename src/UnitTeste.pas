unit UnitTeste;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DBAccess, Uni, Data.DB,
  UniProvider, PostgreSQLUniProvider;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    UniConnection1: TUniConnection;
    UniTransaction1: TUniTransaction;
    PostgreSQLUniProvider1: TPostgreSQLUniProvider;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Pais, Atributos, Base, DaoUni;

procedure TForm1.Button1Click(Sender: TObject);
var
  ATab: TPais;
begin
  ATab := TPais.Create;
  try
    Edit1.Text := '';
    Edit1.Text := PegaNomeTab(Atab);
  finally
    atab.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ATab: TPais;
  Pk: TResultArray;
  chave: String;
begin
  ATab := TPais.Create;
  try
    Pk := PegaPks(Atab);
    Edit1.Text := '';
    for chave in Pk do
    begin
      Edit1.Text := Edit1.Text + ' ' + chave;
    end;
  finally
    ATab.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Pais: TPais;
  Dao: IDaoBase;
  Registros: Integer;
begin
  Pais := TPais.Create;
  Dao := TDaoUni.Create(UniConnection1, UniTransaction1);
  try
    Pais.Id := 1;
    Pais.Nome := 'Brasil';
    Registros := Dao.Inserir(Pais);
  finally
    Pais.Free;
  end;
end;

end.
