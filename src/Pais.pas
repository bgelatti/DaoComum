unit Pais;

interface

uses
  Atributos, System.Rtti, Base;

type
  [TNomeTabela('COMUM.PAIS')]
  TPais = class(TTabela)
  private
    FId: Integer;
    FNome: string;
    procedure SetId(const Value: Integer);
    procedure SetNome(const Value: string);
  public
    [TCampoPk]
    property Id: Integer read FId write SetId;
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
