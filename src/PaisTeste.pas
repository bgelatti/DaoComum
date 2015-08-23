unit Pais;

interface

uses
  Atributos, System.Rtti, Base, Table;

type
  [AttTable('COMUM.PAIS')]
  TPais = class(TTable)
  private
    FId: Integer;
    FNome: string;
    procedure SetId(const Value: Integer);
    procedure SetNome(const Value: string);
  public
    [AttPK]
    [AttNotNull('Código do Pais')]
    property Id: Integer read FId write SetId;
    [AttNotNull('Nome do Pais')]
    property Nome: string read FNome write SetNome;
  end;

implementation


{ TPais }

procedure TPais.SetId(const Value: Integer);
begin
  FId := Value;
end;

procedure TPais.SetNome(const Value: string);
begin
  FNome := Value;
end;

end.
