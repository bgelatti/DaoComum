unit RecParams;

interface

uses
  System.Rtti, Table;

type
  TRecParams = record
    Prop: TRttiProperty;
    Field: string;
    Table: TTable;
    Qry: TObject;
  end;

implementation

end.
