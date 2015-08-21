unit BaseConnection;

interface

type
  TBaseConnection = class
  private
    FPass: string;
    FPrt: Integer;
    FUser: string;
    FLocalBD: string;
    FProvider: String;
    FServ: String;
    procedure SetLocalBD(const Value: string);
    procedure SetPass(const Value: string);
    procedure SetProvider(const Value: String);
    procedure SetPrt(const Value: Integer);
    procedure SetServ(const Value: String);
    procedure SetUser(const Value: string);
  public
    function Connected: Boolean; virtual; abstract;
    procedure Connect; virtual; abstract;
    property LocalBD: string read FLocalBD write SetLocalBD;
    property User: string read FUser write SetUser;
    property Pass: string read FPass write SetPass;
    property Serv: String read FServ write SetServ;
    property Provider: String read FProvider write SetProvider;
    property Prt: Integer read FPrt write SetPrt;
  end;

implementation

{ TBaseConnection }

procedure TBaseConnection.SetLocalBD(const Value: string);
begin
  FLocalBD := Value;
end;

procedure TBaseConnection.SetPass(const Value: string);
begin
  FPass := Value;
end;

procedure TBaseConnection.SetProvider(const Value: String);
begin
  FProvider := Value;
end;

procedure TBaseConnection.SetPrt(const Value: Integer);
begin
  FPrt := Value;
end;

procedure TBaseConnection.SetServ(const Value: String);
begin
  FServ := Value;
end;

procedure TBaseConnection.SetUser(const Value: string);
begin
  FUser := Value;
end;

end.
