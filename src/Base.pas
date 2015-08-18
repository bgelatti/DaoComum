unit Base;

interface

type
  IBaseDados = interface
  ['{F4995A89-58FD-4416-AD13-8C4881E1DD1A}']
  end;

  ITransacao = interface
  ['{717EAB8A-2C07-4878-8468-CE62C08371FB}']
  end;

  TTabela = class(TObject)
  end;

  IDaoBase = interface
    ['{A987065E-33C8-4848-8425-436549B25884}']

    function Inserir(ATabela: TTabela): Integer;
    function Salvar(ATabela: TTabela): Integer;
    function Excluir(ATabela: TTabela): Integer;

    function InTransaction: Boolean;
    procedure StartTransaction;
    procedure Commit;
    procedure RollBack;
  end;

implementation

end.
