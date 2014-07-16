program BSAExtractor;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  MorrowindBSA in 'MorrowindBSA.pas',
  OtherBSA in 'OtherBSA.pas',
  AbstractBSAWorker in 'AbstractBSAWorker.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
