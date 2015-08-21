unit DaoUni;

interface

uses Db, Base, Rtti, Atributos, system.SysUtils, system.Classes,
  system.Generics.Collections, Uni, PostgreSQLUniProvider, BaseConnection,
  BaseTransaction, TransactionUni, RecParams, Table, ConnectionUni;

type
  TDaoUni = class(TBaseDao)
  private
    FConnection: TConnectionUni;
    Qry: TUniQuery;
    Function DbToTable<T: TTable>(ATable: TTable; ADataSet: TDataSet): TObjectList<T>;
  protected
    procedure QryParamInteger(ARecParams: TRecParams); override;
    procedure QryParamString(ARecParams: TRecParams); override;
    procedure QryParamDate(ARecParams: TRecParams); override;
    procedure QryParamCurrency(ARecParams: TRecParams); override;
    procedure QryParamVariant(ARecParams: TRecParams); override;
    procedure SetFieldsInteger(ARecParams: TRecParams); override;
    procedure SetFieldsString(ARecParams: TRecParams); override;
    procedure SetFieldsDate(ARecParams: TRecParams); override;
    procedure SetFieldsCurrency(ARecParams: TRecParams); override;
    function ExecuteQuery: Integer; override;
  public
    constructor Create(AConnection: TConnectionUni; ATransaction: TTransactionUni);
    destructor Destroy; override;
    function SearchSQL(ASQL: string): TDataSet; override;
    function SearchTab(ATable: TTable; AFields: array of string): TDataSet; override;
    function SearchGen<T: TTable>(ATable: TTable; AFields: array of string): TObjectList<T>;
    function GetID(ATable: TTable; AField: string): Integer; override;
    function GetMax(ATable: TTable; AField: string; AFieldsChave: array of string): Integer;
    function GetRecordCount(ATable: TTable; AFields: array of string): Integer; override;
    function Insert(ATable: TTable): Integer; override;
    function Update(ATable: TTable): Integer; override;
    function Delete(ATable: TTable): Integer; override;
    function Read(ATable: TTable): Integer; override;
  end;

implementation

uses
  system.TypInfo;

{ TDaoUni }

constructor TDaoUni.Create(AConnection: TConnectionUni; ATransaction: TTransactionUni);
var
  MyDataSet: TUniQuery;
begin
  inherited Create;

  FConnection := AConnection;
  with FConnection do
  begin
    TransQuery := TUniTransaction.Create(nil);

    with TransQuery do
    begin
      DefaultConnection := Database;
    end;
    Database.DefaultTransaction := TransQuery;
  end;

  Qry := TUniQuery.Create(nil);
  Qry.Connection := FConnection.Database;
  Qry.Transaction := ATransaction.Transaction;

  MyDataSet := TUniQuery.Create(nil);
  MyDataSet.Connection := FConnection.Database;

  DataSet := MyDataSet;
end;

destructor TDaoUni.Destroy;
begin
  inherited;
end;

procedure TDaoUni.QryParamCurrency(ARecParams: TRecParams);
begin
  inherited;
  with ARecParams do
  begin
    TUniQuery(Qry).ParamByName(Field).AsCurrency := Prop.GetValue(Table).AsCurrency;
  end;
end;

procedure TDaoUni.QryParamDate(ARecParams: TRecParams);
begin
  inherited;
  with ARecParams do
  begin
    if Prop.GetValue(Table).AsType<TDateTime> = 0 then
    begin
      TUniQuery(Qry).ParamByName(Field).Clear;
    end
    else
    begin
      TUniQuery(Qry).ParamByName(Field).AsDateTime := Prop.GetValue(Table).AsType<TDateTime>;
    end;
  end;
end;

procedure TDaoUni.QryParamInteger(ARecParams: TRecParams);
begin
  inherited;
  with ARecParams do
  begin
    TUniQuery(Qry).ParamByName(Field).AsInteger := Prop.GetValue(Table).AsInteger;
  end;
end;

procedure TDaoUni.QryParamString(ARecParams: TRecParams);
begin
  inherited;
  with ARecParams do
  begin
    TUniQuery(Qry).ParamByName(Field).AsString := Prop.GetValue(Table).AsString;
  end;
end;

procedure TDaoUni.QryParamVariant(ARecParams: TRecParams);
begin
  inherited;
  with ARecParams do
  begin
    TUniQuery(Qry).ParamByName(Field).Value := Prop.GetValue(Table).AsVariant;
  end;
end;

procedure TDaoUni.SetFieldsCurrency(ARecParams: TRecParams);
begin
  inherited;
  with ARecParams do
  begin
    Prop.SetValue(Table, TUniQuery(Qry).FieldByName(Field).AsCurrency);
  end;
end;

procedure TDaoUni.SetFieldsDate(ARecParams: TRecParams);
begin
  inherited;
  with ARecParams do
  begin
    Prop.SetValue(Table, TUniQuery(Qry).FieldByName(Field).AsDateTime);
  end;
end;

procedure TDaoUni.SetFieldsInteger(ARecParams: TRecParams);
begin
  inherited;
  with ARecParams do
  begin
    Prop.SetValue(Table, TUniQuery(Qry).FieldByName(Field).AsInteger);
  end;
end;

procedure TDaoUni.SetFieldsString(ARecParams: TRecParams);
begin
  inherited;
  with ARecParams do
  begin
    Prop.SetValue(Table, TUniQuery(Qry).FieldByName(Field).AsString);
  end;
end;

function TDaoUni.DbToTable<T>(ATable: TTable; ADataSet: TDataSet): TObjectList<T>;
var
  AuxValue: TValue;
  RttiType: TRttiType;
  Context: TRttiContext;
  PropRtti: TRttiProperty;
  DataType: TFieldType;
  Field: String;
begin
  Result := TObjectList<T>.Create;

  while not ADataSet.Eof do
  begin
    AuxValue := GetTypeData(PTypeInfo(TypeInfo(T)))^.ClassType.Create;
    RttiType := Context.GetType(AuxValue.AsObject.ClassInfo);
    for PropRtti in RttiType.GetProperties do
    begin
      Field := PropRtti.Name;
      DataType := ADataSet.FieldByName(Field).DataType;
      case DataType of
        ftInteger:
        begin
          PropRtti.SetValue(AuxValue.AsObject,
            TValue.FromVariant(ADataSet.FieldByName(Field).AsInteger));
        end;
        ftString, ftWideString, ftWideMemo:
        begin
          PropRtti.SetValue(AuxValue.AsObject,
            TValue.FromVariant(ADataSet.FieldByName(Field).AsString));
        end;
        ftBCD, ftFloat:
        begin
          PropRtti.SetValue(AuxValue.AsObject,
            TValue.FromVariant(ADataSet.FieldByName(Field).AsFloat));
        end;
        ftDate, ftDateTime:
        begin
          PropRtti.SetValue(AuxValue.AsObject,
            TValue.FromVariant(ADataSet.FieldByName(Field).AsDateTime));
        end;
      else
        raise Exception.Create('Tipo de Campo não conhecido: ' +
          PropRtti.PropertyType.ToString);
      end;
    end;
    Result.Add(AuxValue.AsType<T>);
    ADataSet.Next;
  end;
end;

function TDaoUni.SearchGen<T>(ATable: TTable; AFields: array of string): TObjectList<T>;
var
  Data: TUniQuery;
  Context: TRttiContext;
  Field: string;
  RttiType: TRttiType;
  PropRtti: TRttiProperty;
begin
  Data := TUniQuery.Create(nil);
  try
    Context := TRttiContext.Create;
    try
      RttiType := Context.GetType(ATable.ClassType);
      with Data do
      begin
        Connection := FConnection.Database;
        SQL.Text := GenerateSQLSelect(ATable, AFields);
        for Field in AFields do
        begin
          if not PropExist(Field, PropRtti, RttiType) then
          begin
            raise Exception.Create('Campo ' + Field + ' não existe no objeto!');
          end;
          for PropRtti in RttiType.GetProperties do
          begin
            if CompareText(PropRtti.Name, Field) = 0 then
            begin
              ConfigureParameter(PropRtti, Field, ATable, Data);
            end;
          end;
        end;
        Open;
        Result := DbToTable<T>(ATable, Data);
      end;
    finally
      Context.Free;
    end;
  finally
    Data.Free;
  end;
end;

function TDaoUni.SearchSQL(ASQL: string): TDataSet;
var
  AQry: TUniQuery;
begin
  AQry := TUniQuery.Create(nil);
  with AQry do
  begin
    Connection := FConnection.Database;
    SQL.Clear;
    SQL.Add(ASQL);
    Open;
  end;
  Result := AQry;
end;

function TDaoUni.SearchTab(ATable: TTable; AFields: array of string): TDataSet;
var
  Data: TUniQuery;
  Context: TRttiContext;
  Field: string;
  RttiType: TRttiType;
  PropRtti: TRttiProperty;
begin
  Data := TUniQuery.Create(nil);
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(ATable.ClassType);
    with Data do
    begin
      Connection := FConnection.Database;
      SQL.Text := GenerateSQLSelect(ATable, AFields);
      for Field in AFields do
      begin
        for PropRtti in RttiType.GetProperties do
        begin
          if CompareText(PropRtti.Name, Field) = 0 then
          begin
            ConfigureParameter(PropRtti, Field, ATable, Data);
          end;
        end;
      end;
      Open;
      Result := Data;
    end;
  finally
    Context.Free;
  end;
end;

function TDaoUni.GetID(ATable: TTable; AField: string): Integer;
var
  AQry: TUniQuery;
begin
  AQry := TUniQuery.Create(nil);
  with AQry do
  begin
    Connection := FConnection.Database;
    SQL.Clear;
    SQL.Add('SELECT MAX(' + AField + ') FROM ' + GetTableName(ATable));
    Open;
    Result := fields[0].AsInteger + 1;
  end;
end;

function TDaoUni.GetMax(ATable: TTable; AField: string;
  AFieldsChave: array of string): Integer;
var
  AQry: TUniQuery;
  Field: string;
  Context: TRttiContext;
  RttiType: TRttiType;
  PropRtti: TRttiProperty;
  Separador: string;
  NumMax: Integer;
begin
  AQry := TUniQuery.Create(nil);
  try
    with AQry do
    begin
      Connection := FConnection.Database;
      SQL.Clear;
      SQL.Add('SELECT MAX(' + AField + ') FROM ' + GetTableName(ATable));
      SQL.Add(' WHERE ');
      Separador := '';
      for Field in AFieldsChave do
      begin
        SQL.Add(Separador + Field + '= :' + Field);
        Separador := ' AND ';
      end;
      Context := TRttiContext.Create;
      try
        RttiType := Context.GetType(ATable.ClassType);
        for Field in AFieldsChave do
        begin
          for PropRtti in RttiType.GetProperties do
          begin
            if CompareText(PropRtti.Name, Field) = 0 then
            begin
              ConfigureParameter(PropRtti, Field, ATable, AQry);
            end;
          end;
        end;
        Open;
        NumMax := Fields[0].AsInteger;
        Result := NumMax;
      finally
        Context.Free;
      end;
    end;
  finally
    AQry.Free;
  end;
end;

function TDaoUni.GetRecordCount(ATable: TTable;
  AFields: array of string): Integer;
var
  AQry: TUniQuery;
  Context: TRttiContext;
  Field: string;
  RttiType: TRttiType;
  PropRtti: TRttiProperty;
begin
  AQry := TUniQuery.Create(nil);
  with AQry do
  begin
    Context := TRttiContext.Create;
    try
      RttiType := Context.GetType(ATable.ClassType);
      Connection := FConnection.Database;
      SQL.Clear;
      SQL.Add('SELECT COUNT(*) FROM ' + GetTableName(ATable));

      if High(AFields) >= 0 then
      begin
        SQL.Add('WHERE 1=1');
      end;

      for Field in AFields do
      begin
        SQL.Add(' AND ' + Field + ' = :' + Field);
      end;

      for Field in AFields do
      begin
        for PropRtti in RttiType.GetProperties do
        begin
          if CompareText(PropRtti.Name, Field) = 0 then
          begin
            ConfigureParameter(PropRtti, Field, ATable, AQry);
          end;
        end;
      end;
      Open;
      Result := fields[0].AsInteger;
    finally
      Context.Free;
    end;
  end;
end;

function TDaoUni.ExecuteQuery: Integer;
begin
  with Qry do
  begin
    Prepare();
    ExecSQL;
    Result := RowsAffected;
  end;
end;

function TDaoUni.Delete(ATable: TTable): Integer;
var
  Command: TReflectionFunc;
begin
  Command := function(AFields: TAnonFields): Integer
  var
    Field: string;
    PropRtti: TRttiProperty;
  begin
    Qry.Close;
    Qry.SQL.Clear;
    Qry.SQL.Text := GenerateSQLDelete(ATable);
    for Field in GetPk(ATable) do
    begin
      for PropRtti in AFields.RttiType.GetProperties do
      begin
        if CompareText(PropRtti.Name, Field) = 0 then
        begin
          ConfigureParameter(PropRtti, Field, ATable, Qry);
        end;
      end;
    end;
    Result := ExecuteQuery;
  end;
  Result := SQLReflection(ATable, Command);
end;

function TDaoUni.Insert(ATable: TTable): Integer;
var
  Command: TReflectionFunc;
begin
  try
    ValidateTable(ATable);

    Command := function(AFields: TAnonFields): Integer
    var
      Field: string;
      PropRtti: TRttiProperty;
    begin
      with Qry do
      begin
        Close;
        SQL.Clear;
        SQL.Text := GenerateSQLInsert(ATable, AFields.RttiType);
        for PropRtti in AFields.RttiType.GetProperties do
        begin
          Field := PropRtti.Name;
          ConfigureParameter(PropRtti, Field, ATable, Qry);
        end;
      end;
      Result := ExecuteQuery;
    end;
    Result := SQLReflection(ATable, Command);
  except
    raise;
  end;
end;

function TDaoUni.Update(ATable: TTable): Integer;
var
  Command: TReflectionFunc;
begin
  try
    ValidateTable(ATable);
    Command := function(AFields: TAnonFields): Integer
    var
      Field: string;
      PropRtti: TRttiProperty;
    begin
      with Qry do
      begin
        Close;
        SQL.Clear;
        SQL.Text := GenerateSQLUpdate(ATable, AFields.RttiType);
        for PropRtti in AFields.RttiType.GetProperties do
        begin
          Field := PropRtti.Name;
          ConfigureParameter(PropRtti, Field, ATable, Qry);
        end;
      end;
      Result := ExecuteQuery;
    end;
    Result := SQLReflection(ATable, Command);
  except
    raise;
  end;
end;

function TDaoUni.Read(ATable: TTable): Integer;
var
  Command: TReflectionFunc;
  Data: TUniQuery;
begin
  Data := TUniQuery.Create(nil);
  try
    Command := function(AFields: TAnonFields): Integer
    var
      Field: string;
      PropRtti: TRttiProperty;
    begin
      with Data do
      begin
        Connection := FConnection.Database;
        SQL.Text := GenerateSQLSelect(ATable);
        for Field in AFields.PKs do
        begin
          for PropRtti in AFields.RttiType.GetProperties do
          begin
            if CompareText(PropRtti.Name, Field) = 0 then
            begin
              ConfigureParameter(PropRtti, Field, ATable, Data);
            end;
          end;
        end;
        Open;
        Result := RecordCount;
        if Result > 0 then
        begin
          for PropRtti in AFields.RttiType.GetProperties do
          begin
            Field := PropRtti.Name;
            SetTableData(PropRtti, Field, ATable, Data);
          end;
        end;
      end;
    end;
    Result := SQLReflection(ATable, Command);
  finally
    Data.Free;
  end;
end;

end.
