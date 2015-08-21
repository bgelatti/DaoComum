unit TransactionUni;

interface

uses
  Uni, BaseTransaction;

type
  TTransactionUni = class(TBaseTransaction)
  private
    FTransaction: TUniTransaction;
  public
    constructor Create(ABase: TUniConnection);
    destructor Destroy; override;
    function InTransaction: Boolean; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
    property Transaction: TUniTransaction read FTransaction write FTransaction;
  end;

implementation

{ TTransactionUni }

constructor TTransactionUni.Create(ABase: TUniConnection);
begin
  inherited Create;

  FTransaction := TUniTransaction.Create(nil);
  with FTransaction do
  begin
    DefaultConnection := ABase;
  end;
end;

destructor TTransactionUni.Destroy;
begin
  inherited;
end;

function TTransactionUni.InTransaction: Boolean;
begin
  Result := FTransaction.Active;
end;

procedure TTransactionUni.StartTransaction;
begin
  if not FTransaction.Active then
  begin
    FTransaction.StartTransaction;
  end;
end;

procedure TTransactionUni.RollBack;
begin
  FTransaction.RollBack;
end;

procedure TTransactionUni.Commit;
begin
  FTransaction.Commit;
end;

end.
