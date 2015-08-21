unit BaseTransaction;

interface

type
  TBaseTransaction = class
  public
    function InTransaction: Boolean; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure RollBack; virtual; abstract;
  end;

implementation

end.
