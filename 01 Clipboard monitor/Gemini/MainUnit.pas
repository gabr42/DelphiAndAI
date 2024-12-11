unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Clipbrd, System.IOUtils, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;  // For visualizing the log (optional)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    private
      FClipboardText: string;
      FLogFile: string;
    public
      procedure LogClipboardText(const AText: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FClipboardText := ''; // Initialize
  FLogFile := TPath.Combine(TPath.GetDocumentsPath, 'clipboard.log'); // Log file path

   //Optional: Add a header to the log file
  LogClipboardText('--- Clipboard Log Started ---');

  Timer1.Enabled := True; // Start monitoring
end;

procedure TForm1.LogClipboardText(const AText: string);
var
  LogFile: TStreamWriter;
begin
  try
    LogFile := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8); // Append mode, UTF-8 encoding
    try
      LogFile.WriteLine(DateTimeToStr(Now) + ': ' + AText);
      //Optional: visualize in the memo
      Memo1.Lines.Add(DateTimeToStr(Now) + ': ' + AText);
    finally
      LogFile.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Error writing to log file: ' + E.Message);
    end;
  end;

end;



procedure TForm1.Timer1Timer(Sender: TObject);
var
  CurrentClipboardText: string;
begin
  // Check if the clipboard contains text
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    CurrentClipboardText := Clipboard.AsText;

    // Check if the clipboard content has changed
    if CurrentClipboardText <> FClipboardText then
    begin
      FClipboardText := CurrentClipboardText;
      LogClipboardText(FClipboardText);
    end;

  end else begin
    //If the clipboard is cleared, or non-text is there, we can optionally reset our stored clipboard text,
    //depending on whether we want to log the "clearing" or not.
    FClipboardText := '';  //
  end;

end;

end.
