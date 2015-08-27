unit Base;

interface

uses DB, SysUtils, Classes, Rtti, System.TypInfo, System.Generics.Collections,
  RecParams, Table, ResultArray;

type
  TBaseDao = class(TObject)
  private
    procedure SetDataSet(const Value: TDataSet);
  protected
    FDataSet: TDataSet;
    procedure QryParamInteger(ARecParams: TRecParams); virtual; abstract;
    procedure QryParamString(ARecParams: TRecParams); virtual; abstract;
    procedure QryParamDate(ARecParams: TRecParams); virtual; abstract;
    procedure QryParamCurrency(ARecParams: TRecParams); virtual; abstract;
    procedure QryParamVariant(ARecParams: TRecParams); virtual; abstract;
    procedure SetFieldsInteger(ARecParams: TRecParams); virtual; abstract;
    procedure SetFieldsString(ARecParams: TRecParams); virtual; abstract;
    procedure SetFieldsDate(ARecParams: TRecParams); virtual; abstract;
    procedure SetFieldsCurrency(ARecParams: TRecParams); virtual; abstract;
    procedure ConfigureParameter(AProp: TRttiProperty; AField: string; ATable: TTable; AQry: TObject); virtual;
    procedure SetTableData(AProp: TRttiProperty; AField: string; ATable: TTable; AQry: TObject);
    function ExecuteQuery: Integer; virtual; abstract;
    function PropExist(AField: string; Prop: TRttiProperty; RttiType: TRttiType): Boolean;
    function GenerateSqlInsert(ATable: TTable; TipoRtti: TRttiType): string; virtual;
    function GenerateSqlUpdate(ATable: TTable; TipoRtti: TRttiType): string; virtual;
    function GenerateSqlDelete(ATable: TTable): string; virtual;
    function GenerateSqlSelect(ATable: TTable): string; overload; virtual;
    function GenerateSqlSelect(ATable: TTable; AFields: array of string): string; overload; virtual;
  public
    function FieldsPK(ATable: TTable): TResultArray;
    function SearchSql(ASql: string): TDataSet; virtual; abstract;
    function SearchTab(ATable: TTable; AFields: array of string): TDataSet; virtual; abstract;
    function GetID(ATable:TTable; AField: string): Integer; virtual; abstract;
    function GetRecordCount(ATable: TTable; AFields: array of string): Integer; virtual; abstract;
    function Insert(ATable: TTable): Integer; virtual; abstract;
    function Update(ATable: TTable): Integer; virtual; abstract;
    function Delete(ATable: TTable): Integer; virtual; abstract;
    function Read(ATable: TTable): Integer; virtual; abstract;
    property DataSet: TDataSet read FDataSet write SetDataSet;
  end;

implementation

uses Atributos;

{ DaoBase }
procedure TBaseDao.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
end;

function TBaseDao.FieldsPK(ATable: TTable): TResultArray;
begin
  Result := GetPk(ATable);
end;

procedure TBaseDao.ConfigureParameter(AProp: TRttiProperty; AField: string;
  ATable: TTable; AQry: TObject);
var
  Params: TRecParams;
begin
  Params.Prop := AProp;
  Params.Field := AField;
  Params.Table := ATable;
  Params.Qry := AQry;

  case AProp.PropertyType.TypeKind of
    tkInt64, tkInteger:
      QryParamInteger(Params);
    tkChar, tkString, tkUString:
      QryParamString(Params);
    tkFloat:
      begin
        if CompareText(AProp.PropertyType.Name, 'TDateTime') = 0 then
        begin
          QryParamDate(Params)
        end
        else
        begin
          QryParamCurrency(Params);
        end;
      end;
    tkVariant:
    begin
      QryParamVariant(Params);
    end
  else
    raise Exception.Create('Tipo de Campo não conhecido: ' +
      AProp.PropertyType.ToString);
  end;
end;

function TBaseDao.GenerateSqlDelete(ATable: TTable): string;
var
  Field, Sep: string;
  ASql: TStringList;
begin
  ASql := TStringList.Create;
  try
    with ASql do
    begin
      Add('Delete from ' + GetTableName(ATable));
      Add('Where');
      Sep := '';
      for Field in GetPk(ATable) do
      begin
        Add(Sep + Field + '= :' + Field);
        Sep := ' and ';
      end;
    end;
    Result := ASql.Text;
  finally
    ASql.Free;
  end;
end;

function TBaseDao.GenerateSqlInsert(ATable: TTable; TipoRtti: TRttiType): string;
var
  Sep: string;
  ASql: TStringList;
  PropRtti: TRttiProperty;
begin
  ASql := TStringList.Create;
  try
    with ASql do
    begin
      Add('INSERT INTO ' + GetTableName(ATable));
      Add('(');
      Sep := '';
      for PropRtti in TipoRtti.GetProperties do
      begin
        Add(Sep + PropRtti.Name);
        Sep := ',';
      end;
      Add(')');
      Add('VALUES (');
      Sep := '';
      for PropRtti in TipoRtti.GetProperties do
      begin
        Add(Sep + ':' + PropRtti.Name);
        Sep := ',';
      end;
      Add(')');
    end;
    Result := ASql.Text;
  finally
    ASql.Free;
  end;
end;

function TBaseDao.GenerateSqlSelect(ATable: TTable): string;
var
  Field, Sep: string;
  ASql: TStringList;
begin
  ASql := TStringList.Create;
  try
    with ASql do
    begin
      Add('SELECT * FROM ' + GetTableName(ATable));
      Add(' WHERE ');
      Sep := '';
      for Field in GetPk(ATable) do
      begin
        Add(Sep + Field + ' = :' + Field);
        Sep := ' AND ';
      end;
    end;
    Result := ASql.Text;
  finally
    ASql.Free;
  end;
end;

function TBaseDao.GenerateSqlSelect(ATable: TTable; AFields: array of string): string;
var
  Field, Sep: string;
  ASql: TStringList;
begin
  ASql := TStringList.Create;
  try
    with ASql do
    begin
      Add('SELECT * FROM ' + GetTableName(ATable));
      Add('WHERE 1=1');
      Sep := ' AND ';
      for Field in AFields do
      begin
        Add(Sep + Field + ' = :' + Field);
      end;
      Add('ORDER BY');
      Sep := '';
      for Field in GetPk(ATable) do
      begin
        Add(Sep + Field);
        Sep := ',';
      end;
    end;
    Result := ASql.Text;
  finally
    ASql.Free;
  end;
end;

function TBaseDao.GenerateSqlUpdate(ATable: TTable; TipoRtti: TRttiType): string;
var
  Field, Sep: string;
  ASql: TStringList;
  PropRtti: TRttiProperty;
begin
  ASql := TStringList.Create;
  try
    with ASql do
    begin
      Add('UPDATe ' + GetTableName(ATable));
      Add(' SET ');
      Sep := '';
      for PropRtti in TipoRtti.GetProperties do
      begin
        Add(Sep + PropRtti.Name + ' = :' + PropRtti.Name);
        Sep := ',';
      end;
      Add(' WHERE ');
      Sep := '';
      for Field in GetPk(ATable) do
      begin
        Add(Sep + Field + '= :' + Field);
        Sep := ' AND ';
      end;
    end;
    Result := ASql.Text;
  finally
    ASql.Free;
  end;
end;

function TBaseDao.PropExist(AField: string; Prop: TRttiProperty;
  RttiType: TRttiType): Boolean;
begin
  Result := False;
  for Prop in RttiType.GetProperties do
  begin
    if CompareText(Prop.Name, AField) = 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TBaseDao.SetTableData(AProp: TRttiProperty; AField: string;
  ATable: TTable; AQry: TObject);
var
  Params: TRecParams;
begin
  Params.Prop := AProp;
  Params.Field := AField;
  Params.Table := ATable;
  Params.Qry := AQry;

  case AProp.PropertyType.TypeKind of
    tkInt64, tkInteger:
      begin
        SetFieldsInteger(Params);
      end;
    tkChar, tkString, tkUString:
      begin
        SetFieldsString(Params);
      end;
    tkFloat:
      begin
        if CompareText(AProp.PropertyType.Name, 'TDateTime') = 0 then
        begin
          SetFieldsDate(Params);
        end
        else
        begin
          SetFieldsCurrency(Params);
        end;
      end;
  else
    raise Exception.Create('Tipo de Campo não conhecido: ' +
      AProp.PropertyType.ToString);
  end;
end;

end.
