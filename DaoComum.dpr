program DaoComum;

uses
  Vcl.Forms,
  Atributos in 'src\Atributos.pas',
  Base in 'src\Base.pas',
  DaoUni in 'src\DaoUni.pas',
  Pais in 'src\Pais.pas',
  UnitTeste in 'src\UnitTeste.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
