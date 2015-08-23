unit UnitTeste;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DBAccess, Uni, Data.DB,
  UniProvider, PostgreSQLUniProvider, Base, DaoUni, ConnectionUni, TransactionUni;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Button3: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Button4: TButton;
    Button5: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    Conexao: TConnectionUni;
    Dao: TDaoUni;
    Transacao: TTransactionUni;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses PaisTeste, Atributos;

procedure TForm1.Button1Click(Sender: TObject);
var
  ATab: TPais;
begin
  ATab := TPais.Create;
  try
    Edit1.Text := '';
    Edit1.Text := GetTableName(Atab);
  finally
    ATab.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Pais: TPais;
  Registros: Integer;
begin
  Pais := TPais.Create;
  try
    Pais.Id := Dao.GetID(Pais, 'ID');
    Pais.Nome := Edit3.Text;
    Transacao.StartTransaction;
    try
      Registros := Dao.Insert(Pais);
      Transacao.Commit;
    except
      on E: Exception do
      begin
        Transacao.RollBack;
        ShowMessage('Ocorreu um erro ao inserir! ' + e.Message);
      end;
    end;
  finally
    Pais.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Pais: TPais;
  Registros: Integer;
begin
  Pais := TPais.Create;
  try
    Pais.Id := StrToInt(Edit2.Text);
    Pais.Nome := Edit3.Text;
    Transacao.StartTransaction;
    try
      Registros := Dao.Update(Pais);
      Transacao.Commit;
    except
      on E: Exception do
      begin
        Transacao.RollBack;
        ShowMessage('Ocorreu um erro ao atualizar! ' + e.Message);
      end;
    end;
  finally
    Pais.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  Pais: TPais;
  Registros: Integer;
begin
  Pais := TPais.Create;
  try
    Pais.Id := StrToInt(Edit2.Text);
    Transacao.StartTransaction;
    try
      Registros := Dao.Delete(Pais);
      Transacao.Commit;
    except
      on E: Exception do
      begin
        Transacao.RollBack;
        ShowMessage('Ocorreu um erro ao deletar! ' + e.Message);
      end;
    end;
  finally
    Pais.Free;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  Pais: TPais;
  Registros: Integer;
begin
  Pais := TPais.Create;
  try
    Pais.Id := StrToInt(Edit2.Text);
    Registros := Dao.Read(Pais);
    if Registros > 0 then
    begin
      ShowMessage('ID: ' + IntToStr(Pais.Id) + #13 +
                  'NOME: ' + Pais.Nome);
    end
    else
    begin
      ShowMessage('Nenhum registro encontrado');
    end;
  finally
    Pais.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Conexao := TConnectionUni.Create;
  Transacao := TTransactionUni.Create(Conexao.Database);

  with Conexao do
  begin
    LocalBD := 'system';
    Prt := 5432;
    Serv := 'localhost';
    Provider := 'PostgreSQL';
    User := 'sigar';
    Pass := 'gelsigar';
    Connect;
  end;

  Dao := TDaoUni.Create(Conexao, Transacao);
end;

end.
