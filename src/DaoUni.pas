unit DaoUni;

interface

uses
  Base, System.Rtti, Atributos, Uni, System.SysUtils;

type
  TDaoUni = class(TInterfacedObject, IDaoBase)
  private
    FDatabase: TUniConnection;
    FTransacao: TUniTransaction;
    procedure ConfigParametro(AQuery: TUniQuery; AProp: TRttiProperty;
      ACampo: string; ATabela: TTabela);
    procedure FechaQuery;
    function ExecutaQuery: Integer;
  public
    Qry: TUniQuery;
    constructor Create(ADatabase: TUniConnection; ATransacao: TUniTransaction);
    function Inserir(ATabela: TTabela): Integer;
    function Salvar(ATabela: TTabela): Integer;
    function Excluir(ATabela: TTabela): Integer;
    function InTransaction: Boolean;
    procedure StartTransaction;
    procedure Commit;
    procedure RollBack;
  end;

implementation

{ TDaoUni }

procedure TDaoUni.Commit;
begin
  FTransacao.Commit;
end;

procedure TDaoUni.ConfigParametro(AQuery: TUniQuery; AProp: TRttiProperty;
  ACampo: string; ATabela: TTabela);
begin
  with AQuery do
  begin
    case AProp.PropertyType.TypeKind of
      tkInt64, tkInteger:
      begin
        Params.ParamByName(ACampo).AsInteger := AProp.GetValue(ATabela).AsInteger;
      end;
      tkChar, tkString, tkUString:
      begin
        Params.ParamByName(ACampo).AsString := AProp.GetValue(ATabela).AsString;
      end;
      tkFloat:
      begin
        if CompareText(AProp.PropertyType.Name, 'TDateTime') = 0 then
        begin
          Params.ParamByName(ACampo).AsDateTime := Aprop.GetValue(ATabela).AsType<TDateTime>;
        end
        else
        begin
          Params.ParamByName(ACampo).AsFloat := AProp.GetValue(ATabela).AsCurrency;
        end;
      end;
      tkVariant:
      begin
        Params.ParamByName(ACampo).Value := AProp.GetValue(ATabela).AsVariant;
      end;
    else
      raise Exception.Create('Tipo de campo desconhecido: ' + AProp.PropertyType.ToString);
    end;
  end;
end;

constructor TDaoUni.Create(ADatabase: TUniConnection;
  ATransacao: TUniTransaction);
begin
  inherited Create;
  if not Assigned(ADatabase) then
  begin
    raise Exception.Create('Database não informada!');
  end;

  if not Assigned(ATransacao) then
  begin
    raise Exception.Create('Transação não informada!');
  end;

  FDatabase := ADatabase;
  FTransacao := ATransacao;

  Qry := TUniQuery.Create(nil);
  Qry.Connection := FDatabase;
  Qry.Transaction := FTransacao;
end;

function TDaoUni.Excluir(ATabela: TTabela): Integer;
var
  Comando: TFuncReflexao;
begin
  Comando := function(ACampos: TCamposAnoni): Integer
  var
    Campo: string;
    PropRtti: TRttiProperty;
  begin
    FechaQuery;
    with Qry do
    begin
      SQL.Add('DELETE FROM ' + ACampos.NomeTabela);
      SQL.Add(' WHERE ');
      ACampos.Sep := '';
      for Campo in ACampos.Pks do
      begin
        SQL.Add(ACampos.Sep + Campo + ' = :' + Campo);
        ACampos.Sep := ' AND ';
        for PropRtti in ACampos.TipoRtti.GetProperties do
        begin
          if CompareText(PropRtti.Name, Campo) = 0 then
          begin
            ConfigParametro(Qry, PropRtti, Campo, ATabela);
          end;
        end;
      end;
    end;
    Result := ExecutaQuery;
  end;
  Result := ReflexaoSQL(ATabela, Comando);
end;

function TDaoUni.ExecutaQuery: Integer;
begin
  Qry.Prepare;
  Qry.ExecSQL;
  Result := Qry.RowsAffected;
end;

procedure TDaoUni.FechaQuery;
begin
  Qry.Close;
  Qry.SQL.Clear;
end;

function TDaoUni.Inserir(ATabela: TTabela): Integer;
var
  Comando: TFuncReflexao;
begin
  Comando := function(ACampos: TCamposAnoni): Integer
  var
    Campo: string;
    PropRtti: TRttiProperty;
  begin
    FechaQuery;
    with Qry do
    begin
      SQL.Add('INSERT INTO ' + ACampos.NomeTabela);
      SQL.Add(' ( ');
      ACampos.Sep := '';
      for PropRtti in ACampos.TipoRtti.GetProperties do
      begin
        SQL.Add(ACampos.Sep + PropRtti.Name);
        ACampos.Sep := ',';
      end;
      SQL.Add(' ) ');

      SQL.Add('VALUES ( ');
      ACampos.Sep := '';
      for PropRtti in ACampos.TipoRtti.GetProperties do
      begin
        SQL.Add(ACampos.Sep + ':' + PropRtti.Name);
        ACampos.Sep := ',';
      end;
      SQL.Add(' ) ');

      for PropRtti in ACampos.TipoRtti.GetProperties do
      begin
        Campo := PropRtti.Name;
        ConfigParametro(Qry, PropRtti, Campo, ATabela);
      end;
    end;
    Result := ExecutaQuery;
  end;
  Result := ReflexaoSQL(ATabela, Comando);
end;

function TDaoUni.InTransaction: Boolean;
begin
  Result := FTransacao.Active;
end;

procedure TDaoUni.RollBack;
begin
  FTransacao.Rollback;
end;

function TDaoUni.Salvar(ATabela: TTabela): Integer;
var
  Comando: TFuncReflexao;
begin
  Comando := function(ACampos: TCamposAnoni): Integer
  var
    Campo: string;
    PropRtti: TRttiProperty;
  begin
    FechaQuery;
    with Qry do
    begin
      SQL.Add('UPDATE ' + ACampos.NomeTabela);
      SQL.Add(' SET ');
      ACampos.Sep := '';
      for PropRtti in ACampos.TipoRtti.GetProperties do
      begin
        SQL.Add(ACampos.Sep + PropRtti.Name + ' = :' + PropRtti.Name);
        ACampos.Sep := ',';
      end;
      SQL.Add(' WHERE ');

      ACampos.Sep := '';
      for Campo in ACampos.Pks do
      begin
        SQL.Add(ACampos.Sep + Campo + ' = :' + Campo);
        ACampos.Sep := ' AND ';
      end;

      for PropRtti in ACampos.TipoRtti.GetProperties do
      begin
        Campo := PropRtti.Name;
        ConfigParametro(Qry, PropRtti, Campo, ATabela);
      end;
    end;
    Result := ExecutaQuery;
  end;
  Result := ReflexaoSQL(ATabela, Comando);
end;

procedure TDaoUni.StartTransaction;
begin
  FTransacao.StartTransaction;
end;

end.
