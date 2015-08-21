unit ConnectionUni;

interface

uses
  BaseConnection, Uni;

type
  TConnectionUni = class(TBaseConnection)
  private
    FDatabase: TUniConnection;
    FTransQuery: TUniTransaction;
  public
    constructor Create();
    destructor Destroy; override;
    function Connected: Boolean; override;
    procedure Connect; override;
    property Database: TUniConnection read FDatabase write FDatabase;
    property TransQuery: TUniTransaction read FTransQuery write FTransQuery;
  end;

implementation

{ TConnectionUni }

constructor TConnectionUni.Create();
begin
  inherited Create;
  FDatabase := TUniConnection.Create(nil);
  FDatabase.LoginPrompt := false;
end;

destructor TConnectionUni.Destroy;
begin
  inherited;
end;

function TConnectionUni.Connected: Boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TConnectionUni.Connect;
begin
  inherited;
  with FDatabase do
  begin
    Database := LocalBD;
    Port := Prt;
    Server := Serv;
    ProviderName := Provider;
    Username := User;
    Password := Pass;
    Connected := True;
  end;
end;

end.
