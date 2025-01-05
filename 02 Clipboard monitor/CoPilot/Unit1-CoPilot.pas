unit Unit1;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Clipbrd;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FLastClipboardText: string;
    procedure CheckClipboard;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLastClipboardText := '';
  Timer1.Interval := 1000; // Check every second
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  CheckClipboard;
end;

procedure TForm1.CheckClipboard;
var
  ClipboardText: string;
  LogFile: TextFile;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    ClipboardText := Clipboard.AsText;
    if ClipboardText <> FLastClipboardText then
    begin
      FLastClipboardText := ClipboardText;
      // Append to log file
      AssignFile(LogFile, 'ClipboardLog.txt');
      if FileExists('ClipboardLog.txt') then
        Append(LogFile)
      else
        Rewrite(LogFile);
      try
        Writeln(LogFile, DateTimeToStr(Now) + ': ' + ClipboardText);
      finally
        CloseFile(LogFile);
      end;
      // Optionally, display in memo
      Memo1.Lines.Add(DateTimeToStr(Now) + ': ' + ClipboardText);
    end;
  end;
end;
