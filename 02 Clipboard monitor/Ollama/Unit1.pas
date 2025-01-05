unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Clipbrd,
  JvClipboardMonitor;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    ClipMon: TJvClipboardMonitor; // *** added field
    procedure OnClipboardChange(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create a new instance of the TJvClipMonitor component
  ClipMon := TJvClipboardMonitor.Create(Self); // *** used correct class name

  // Set up the clipboard monitoring options
//  ClipMon.ClipboardFormat := cfText; // Only monitor text format              // *** removed - there is no ClipboardFormat property
  ClipMon.OnChange := OnClipboardChange; // Call the OnClipboardChange event when the clipboard changes
end;

procedure TForm1.OnClipboardChange(Sender: TObject);
begin
  // Append the current clipboard content to a log file
  with TStringList.Create do
    try
      Add('Clipboard contents: ' + Clipboard.AsText); // *** use Clipboard.AsText instead of ClipMon.GetText
      SaveToFile('clipboard_log.txt');
    finally
      Free;
    end;
end;

end.
