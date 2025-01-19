program Encoder;

uses
  Vcl.Forms,
  EncoderMain in 'EncoderMain.pas' {Form14};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm14, Form14);
  Application.Run;
end.
