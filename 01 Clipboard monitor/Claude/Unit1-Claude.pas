unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmMain = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    mmoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    FNextViewer: HWND;
    FLogFileName: string;
    FMonitoring: Boolean;
    procedure WMChangeCBChain(var Msg: TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard(var Msg: TMessage); message WM_DRAWCLIPBOARD;
    procedure LogClipboardContent;
    procedure StartMonitoring;
    procedure StopMonitoring;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FLogFileName := ChangeFileExt(Application.ExeName, '.log');
  FMonitoring := False;
  btnStop.Enabled := False;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if FMonitoring then
    StopMonitoring;
end;

procedure TfrmMain.StartMonitoring;
begin
  FNextViewer := SetClipboardViewer(Handle);
  FMonitoring := True;
  btnStart.Enabled := False;
  btnStop.Enabled := True;
  mmoLog.Lines.Add('Monitoring started...');
end;

procedure TfrmMain.StopMonitoring;
begin
  ChangeClipboardChain(Handle, FNextViewer);
  FMonitoring := False;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
  mmoLog.Lines.Add('Monitoring stopped...');
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  StartMonitoring;
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  StopMonitoring;
end;

procedure TfrmMain.WMChangeCBChain(var Msg: TWMChangeCBChain);
begin
  if Msg.Remove = FNextViewer then
    FNextViewer := Msg.Next
  else if FNextViewer <> 0 then
    SendMessage(FNextViewer, WM_CHANGECBCHAIN, Msg.Remove, Msg.Next);

  Msg.Result := 0;
end;

procedure TfrmMain.WMDrawClipboard(var Msg: TMessage);
begin
  if FMonitoring then
  begin
    LogClipboardContent;

    // Pass the message to the next viewer in the chain
    if FNextViewer <> 0 then
      SendMessage(FNextViewer, WM_DRAWCLIPBOARD, 0, 0);
  end;
end;

procedure TfrmMain.LogClipboardContent;
var
  LogFile: TextFile;
  ClipText: string;
begin
  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      ClipText := Clipboard.AsText;

      // Log to file
      AssignFile(LogFile, FLogFileName);
      if FileExists(FLogFileName) then
        Append(LogFile)
      else
        Rewrite(LogFile);
      try
        WriteLn(LogFile, Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ClipText]));
      finally
        CloseFile(LogFile);
      end;

      // Show in memo
      mmoLog.Lines.Add(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ClipText]));
    end;
  except
    on E: Exception do
      mmoLog.Lines.Add('Error: ' + E.Message);
  end;
end;

end.

