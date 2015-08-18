unit Atributos;

interface

uses
  System.Rtti, Base;

type
  TResultArray = Array of string;

  TCamposAnoni = record
    NomeTabela: String;
    Sep: String;
    Pks: TResultArray;
    TipoRtti: TRttiType;
  end;

  TFuncReflexao = reference to function(ACampos: TCamposAnoni): Integer;

  TNomeTabela = class(TCustomAttribute)
  private
    FNomeTabela: string;
  public
    constructor Create(ANomeTabela: string);
    property NomeTabela: string read FNomeTabela write FNomeTabela;
  end;

  TCampos = class(TCustomAttribute)
  public
    function IsPk: Boolean; virtual;
  end;

  TCampoPk = class(TCampos)
  public
    function IsPk: Boolean; override;
  end;

  function ReflexaoSQL(ATabela: TTabela; AnoniComando: TFuncReflexao): Integer;
  function PegaNomeTab(AObjeto: TObject): string;
  function PegaPks(AObjeto: TObject): TResultArray;

implementation

function PegaNomeTab(AObjeto: TObject): string;
var
  Contexto: TRttiContext;
  TipoRtti: TRttiType;
  AtribRtti: TCustomAttribute;
begin
  Contexto := TRttiContext.Create;
  try
    TipoRtti := Contexto.GetType(AObjeto.ClassType);

    for AtribRtti in TipoRtti.GetAttributes do
    begin
      if AtribRtti is TNomeTabela then
      begin
        Result := (AtribRtti as TNomeTabela).NomeTabela;
        Break;
      end;
    end;
  finally
    Contexto.Free;
  end;
end;

function PegaPks(AObjeto: TObject): TResultArray;
var
  Contexto: TRttiContext;
  TipoRtti: TRttiType;
  PropRtti: TRttiProperty;
  AtribRtti: TCustomAttribute;
  i: Integer;
begin
  Contexto := TRttiContext.Create;
  try
    TipoRtti := Contexto.GetType(AObjeto.ClassType);
    i := 0;
    for PropRtti in TipoRtti.GetProperties do
    begin
      for AtribRtti in PropRtti.GetAttributes do
      begin
        if AtribRtti Is TCampos then
        begin
          if (AtribRtti as TCampos).IsPk then
          begin
            SetLength(Result, i+1);
            Result[i] := PropRtti.Name;
            inc(i);
          end;
        end;
      end;
    end;
  finally
    Contexto.Free;
  end;
end;

function ReflexaoSQL(ATabela: TTabela; AnoniComando: TFuncReflexao): Integer;
var
  ACampos: TCamposAnoni;
  Contexto: TRttiContext;
begin
  ACampos.NomeTabela := PegaNomeTab(ATabela);
  ACampos.Pks := PegaPks(ATabela);
  Contexto := TRttiContext.Create;
  try
    ACampos.TipoRtti := Contexto.GetType(ATabela.ClassType);
    ACampos.Sep := '';
    Result := AnoniComando(ACampos);
  finally
    Contexto.Free;
  end;
end;

{ TNomeTable }

constructor TNomeTabela.Create(ANomeTabela: string);
begin
  FNomeTabela := ANomeTabela;
end;

{ TCampos }

function TCampos.IsPk: Boolean;
begin
  Result := False;
end;

{ TCampoPk }

function TCampoPk.IsPk: Boolean;
begin
  Result := True;
end;

end.
