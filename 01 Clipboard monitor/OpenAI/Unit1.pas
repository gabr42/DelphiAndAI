unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.IOUtils, Vcl.Clipbrd; // *** added missing units

type
  TForm1 = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure LogToFile(const Text: string);
    procedure HandleClipboardChange;
    procedure WndProc(var Msg: TMessage); override;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  WM_CLIPBOARDUPDATE = $031D;

function AddClipboardFormatListener(hwnd: HWND): BOOL; stdcall; external 'user32.dll' name 'AddClipboardFormatListener';
function RemoveClipboardFormatListener(hwnd: HWND): BOOL; stdcall; external 'user32.dll' name 'RemoveClipboardFormatListener'

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RemoveClipboardFormatListener(Handle);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Register to receive clipboard update messages
  if not AddClipboardFormatListener(Handle) then
    ShowMessage('Failed to add clipboard format listener.');
end;

procedure TForm1.HandleClipboardChange;
var
  ClipboardText: string;
begin
  if Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_UNICODETEXT) then
  begin
    try
      ClipboardText := Clipboard.AsText;
      if ClipboardText.Trim <> '' then
      begin
        LogToFile(ClipboardText);
      end;
    except
      on E: Exception do
        // Optionally handle exceptions, e.g., log to a separate error log
        ShowMessage('Error reading clipboard: ' + E.Message);
    end;
  end;
end;

procedure TForm1.LogToFile(const Text: string);
const
  LogFileName = 'clipboard_log.txt';
var
  LogFilePath: string;
  LogText: string;
begin
  // Define the log file path. Here, it's placed in the same directory as the executable.
  LogFilePath := ExtractFilePath(Application.ExeName) + LogFileName;

  // Prepare the log entry with a timestamp
  LogText := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Text + sLineBreak;

  try
    // Append the log text to the file
    TFile.AppendAllText(LogFilePath, LogText, TEncoding.UTF8);
  except
    on E: Exception do
      // Optionally handle file I/O exceptions
      ShowMessage('Error writing to log file: ' + E.Message);
  end;
end;

procedure TForm1.WndProc(var Msg: TMessage);
begin
  inherited WndProc(Msg);
  if Msg.Msg = WM_CLIPBOARDUPDATE then
  begin
    HandleClipboardChange;
  end;
end;

end.
