unit Atributos;

interface

uses
  Base, Rtti, System.Classes;

type
  TAnonFields = record
    TableName: string;
    Sep: string;
    PKs: TResultArray;
    RttiType: TRttiType;
  end;

  TReflectionFunc = reference to function(AFields: TAnonFields): Integer;

  AttTable = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(ATableName: string);
    property Name: string read FName write FName;
  end;

  AttPK = class(TCustomAttribute)
  end;

  AttBaseValidation = class(TCustomAttribute)
  private
    FErrorMessage: string;
    procedure SetErrorMessage(const Value: string);
  public
    property ErrorMessage: string read FErrorMessage write SetErrorMessage;
  end;

  AttNotNull = class(AttBaseValidation)
  public
    constructor Create(const AFieldName: string);
    function ValidateString(Value: string): Boolean;
    function ValidateInteger(Value: Integer): Boolean;
    function ValidateFloat(Value: Double): Boolean;
    function ValidateData(Value: Double): Boolean;
  end;

  AttMinValue = class(AttBaseValidation)
  private
    FMinimumValue: Double;
  public
    constructor Create(MinimumValue: Double; const AFieldName: string);
    function Validate(Value: Double): Boolean;
  end;

  AttMaxValue = class(AttBaseValidation)
  private
    FMaxValue: Double;
  public
    constructor Create(MaxValue: Double; const AFieldName: string);
    function Validate(Value: Double): Boolean;
  end;

  function SQLReflection(ATable: TTable; AnoniComando: TReflectionFunc): Integer;
  function GetTableName(ATable: TTable): string;
  function GetPk(ATable: TTable): TResultArray;
  procedure ValidateTable(ATable: TTable);
  procedure SetProperty(AObj: TObject; AProp: string; AValor: Variant);

implementation

uses
  System.TypInfo, System.SysUtils, System.Variants;

procedure SetProperty(AObj: TObject; AProp: string; AValor: Variant);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  PropRtti: TRttiProperty;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AObj.ClassType);
    for PropRtti in RttiType.GetProperties do
    begin
      if CompareText(PropRtti.Name, AProp) = 0 then
      begin
        PropRtti.SetValue(AObj, System.Variants.VarToStr(AValor));
      end;
    end;
  finally
    Context.free;
  end;
end;

function SQLReflection(ATable: TTable; AnoniComando: TReflectionFunc): Integer;
var
  AFields: TAnonFields;
  Context: TRttiContext;
begin
  AFields.TableName := GetTableName(ATable);
  if AFields.TableName = EmptyStr then
  begin
    raise Exception.Create('Informe o Atributo NomeTabela na classe ' +
      ATable.ClassName);
  end;

  AFields.PKs := GetPk(ATable);
  if Length(AFields.PKs) = 0 then
  begin
    raise Exception.Create('Informe campos da chave primária na classe ' +
      ATable.ClassName);
  end;

  Context := TRttiContext.Create;
  try
    AFields.RttiType := Context.GetType(ATable.ClassType);
    AFields.Sep := '';
    Result := AnoniComando(AFields);
  finally
    Context.free;
  end;
end;

function GetTableName(ATable: TTable): string;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  AtribRtti: TCustomAttribute;
begin
  Context := TRttiContext.Create;
  RttiType := Context.GetType(ATable.ClassType);
  try
    for AtribRtti in RttiType.GetAttributes do
    begin
      if AtribRtti Is AttTable then
      begin
        Result := (AtribRtti as AttTable).Name;
        Break;
      end;
    end;
  finally
    Context.free;
  end;
end;

procedure ValidateTable(ATable: TTable);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  PropRtti: TRttiProperty;
  AtribRtti: TCustomAttribute;
  ErrorList: TStrings;
begin
  try
    if not Assigned(ATable) then
    begin
      raise Exception.Create('Tabela não foi passada no parâmetro!');
    end;

    ErrorList := TStringList.Create;
    try
      Context := TRttiContext.Create;
      try
        RttiType := Context.GetType(ATable.ClassType);
        for PropRtti in RttiType.GetProperties do
        begin
          for AtribRtti in PropRtti.GetAttributes do
          begin
            PropRtti.PropertyType.TypeKind;
            if AtribRtti is AttMinValue then
            begin
              if not AttMinValue(AtribRtti).Validate(PropRtti.GetValue(ATable).AsExtended) then
              begin
                ErrorList.Add(AttBaseValidation(AtribRtti).ErrorMessage);
              end;
            end;
            if AtribRtti is AttMaxValue then
            begin
              if not AttMinValue(AtribRtti).Validate(PropRtti.GetValue(ATable).AsExtended) then
              begin
                ErrorList.Add(AttBaseValidation(AtribRtti).ErrorMessage);
              end;
            end;

            if AtribRtti is AttNotNull then
            begin
              case PropRtti.PropertyType.TypeKind of
                tkFloat:
                  begin
                    if CompareText(PropRtti.PropertyType.Name, 'TDateTime') = 0
                    then
                    begin
                      if not AttNotNull(AtribRtti).ValidateData(PropRtti.GetValue(ATable).AsExtended) then
                      begin
                        ErrorList.Add(AttBaseValidation(AtribRtti).ErrorMessage);
                      end;
                    end
                    else
                    begin
                      if not AttNotNull(AtribRtti).ValidateFloat(PropRtti.GetValue(ATable).AsExtended) then
                      begin
                        ErrorList.Add(AttBaseValidation(AtribRtti).ErrorMessage);
                      end;
                    end;
                  end;
                tkInteger:
                  begin
                    if not AttNotNull(AtribRtti).ValidateInteger(PropRtti.GetValue(ATable).AsInteger) then
                    begin
                      ErrorList.Add(AttBaseValidation(AtribRtti).ErrorMessage);
                    end;
                  end;
              else
                begin
                  if not AttNotNull(AtribRtti).ValidateString(PropRtti.GetValue(ATable).AsString) then
                  begin
                    ErrorList.Add(AttBaseValidation(AtribRtti).ErrorMessage);
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        Context.free;
      end;

      if ErrorList.Count > 0 then
      begin
        raise Exception.Create(PChar(ErrorList.Text));
      end;
    finally
      ErrorList.free;
    end;
  except
    raise;
  end;
end;

function GetPK(ATable: TTable): TResultArray;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  PropRtti: TRttiProperty;
  AtribRtti: TCustomAttribute;
  i: Integer;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(ATable.ClassType);
    i := 0;
    for PropRtti in RttiType.GetProperties do
      for AtribRtti in PropRtti.GetAttributes do
        if AtribRtti Is AttPK then
        begin
          SetLength(Result, i + 1);
          Result[i] := PropRtti.Name;
          inc(i);
        end;
  finally
    Context.free;
  end;
end;

{ TNomeTabela }

constructor AtTTable.Create(ATableName: string);
begin
  FName := ATableName;
end;

{ TBaseValidacao }

procedure AttBaseValidation.SetErrorMessage(const Value: string);
begin
  FErrorMessage := Value;
end;

{ TValidaIntegerMinimo }

constructor AttMinValue.Create(MinimumValue: Double; const AFieldName: string);
begin
  FMinimumValue := MinimumValue;
  FErrorMessage := 'Campo ' + AFieldName + ' com valor inválido!';
end;

function AttMinValue.Validate(Value: Double): Boolean;
begin
  Result := Value >= FMinimumValue;
end;

constructor AttMaxValue.Create(MaxValue: Double; const AFieldName: string);
begin
  FMaxValue := MaxValue;
  FErrorMessage := 'Campo ' + AFieldName + ' com valor inválido!';
end;

function AttMaxValue.Validate(Value: Double): Boolean;
begin
  Result := Value <= FMaxValue;
end;

{ TValidaStringNaoNulo }

constructor AttNotNull.Create(const AFieldName: string);
begin
  FErrorMessage := 'Campo obrigatório não informado: ' + AFieldName ;
end;

function AttNotNull.ValidateString(Value: string): Boolean;
begin
  Result := not(Value = EmptyStr);
end;

function AttNotNull.ValidateInteger(Value: Integer): Boolean;
begin
  Result := not(Value <= 0);
end;

function AttNotNull.ValidateFloat(Value: Double): Boolean;
begin
  Result := not(Value <= 0);
end;

function AttNotNull.ValidateData(Value: Double): Boolean;
begin
  Result := not(Value = 0);
end;

end.
