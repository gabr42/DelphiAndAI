unit EncoderMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Character,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm14 = class(TForm)
    btnGemini: TButton;
    inpText: TMemo;
    Label1: TLabel;
    outEncoded: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    outDecoded: TMemo;
    btnGeminiFixed: TButton;
    btnClaude: TButton;
    btnClaudeFixed: TButton;
    btnCoPilot: TButton;
    btnCoPilotFixed: TButton;
    btnOpenAI: TButton;
    btnOpenAIFixed: TButton;
    procedure btnClaudeClick(Sender: TObject);
    procedure btnClaudeFixedClick(Sender: TObject);
    procedure btnCoPilotClick(Sender: TObject);
    procedure btnCoPilotFixedClick(Sender: TObject);
    procedure btnGeminiClick(Sender: TObject);
    procedure btnGeminiFixedClick(Sender: TObject);
    procedure btnOpenAIClick(Sender: TObject);
    procedure btnOpenAIFixedClick(Sender: TObject);
  private
  public
  end;

var
  Form14: TForm14;

implementation

uses
  AnsiStrings;

{$R *.dfm}

function Gemini_EncodeNonPrintableChars(const InputStr: string): string;
var
  i: Integer;
  ch: Char;
  EncodedStr: string;
begin
  EncodedStr := '';
  for i := 1 to Length(InputStr) do
  begin
    ch := InputStr[i];
    if (ch >= #32) and (ch <= #126) then // Printable ASCII range
      EncodedStr := EncodedStr + ch
    else
      EncodedStr := EncodedStr + Format('#%2.2x', [Ord(ch)]); // Encode as #XX
  end;
  Result := EncodedStr;
end;

function Gemini_EncodeNonPrintableChars_Fixed(const InputStr: string): string;
var
  i: Integer;
  ch: Char;
  EncodedStr: string;
begin
  EncodedStr := '';
  for i := 1 to Length(InputStr) do
  begin
    ch := InputStr[i];
    if ch = '#' then
      EncodedStr := EncodedStr + '##' // Escape literal # characters
    else if (ch >= #32) and (ch <= #126) then
      EncodedStr := EncodedStr + ch
    else
      EncodedStr := EncodedStr + Format('#%.2x', [Ord(ch)]);
  end;
  Result := EncodedStr;
end;

function Gemini_DecodeNonPrintableChars(const InputStr: string): string;
var
  i: Integer;
  ch: Char;
  DecodedStr: string;
  CodeStr: string;
  Code: Integer;
begin
  DecodedStr := '';
  i := 1;
  while i <= Length(InputStr) do
  begin
    ch := InputStr[i];
    if ch = '#' then
    begin
      if (i + 2) <= Length(InputStr) then // Check for enough characters for #XX
      begin
        CodeStr := InputStr[i+1] + InputStr[i+2];
        try
          Code := StrToInt('$' + CodeStr); // Convert hex string to integer
          DecodedStr := DecodedStr + Char(Code);
          Inc(i, 3); // Skip #XX
        except
          on E: Exception do
          begin
            DecodedStr := DecodedStr + '#'; // Treat as a literal # if conversion fails
            Inc(i);
          end;
        end;
      end else begin
        DecodedStr := DecodedStr + '#'; // Treat as a literal #
        Inc(i);
      end;
    end else
    begin
      DecodedStr := DecodedStr + ch;
      Inc(i);
    end;
  end;
  Result := DecodedStr;
end;

function Gemini_DecodeNonPrintableChars_Fixed(const InputStr: string): string;
var
  i: Integer;
  DecodedStr: string;
  HexStr: string;
  Code: Integer;
begin
  DecodedStr := '';
  i := 1;
  while i <= Length(InputStr) do
  begin
    if (i <= Length(InputStr) -1) and (InputStr[i] = '#') and (InputStr[i+1] = '#') then
    begin
        // Double # means a literal #
        DecodedStr := DecodedStr + '#';
        inc(i,2); // Skip both # characters
    end
    else if InputStr[i] = '#' then
    begin
      Inc(i); // Skip '#'
      HexStr := '';
       while (i <= Length(InputStr)) and
            (InputStr[i] in ['0'..'9', 'A'..'F', 'a'..'f']) do
      begin
        HexStr := HexStr + InputStr[i];
        Inc(i);
      end;
      if HexStr <> '' then
      begin
        try
          Code := StrToInt('$' + HexStr);
          DecodedStr := DecodedStr + Char(Code);
        except
           DecodedStr := DecodedStr + '#' + HexStr; // Handle as literal if invalid
        end;
      end else begin
          DecodedStr := DecodedStr + '#'; // Handle '#' alone
      end;
    end else
    begin
      DecodedStr := DecodedStr + InputStr[i];
      Inc(i);
    end;
  end;
  Result := DecodedStr;
end;

function Claude_EncodeToSingleLine(const AText: string): string;
const
  MinPrintable = 32;  // Space character
  MaxPrintable = 126; // Last printable ASCII character
begin
  Result := '';
  for var I := 1 to Length(AText) do
  begin
    var Ch := AText[I];
    if (Ord(Ch) >= MinPrintable) and (Ord(Ch) <= MaxPrintable) then
      Result := Result + Ch
    else
      Result := Result + '\x' + IntToHex(Ord(Ch), 2);
  end;
end;

function Claude_DecodeFromSingleLine(const AText: string): string;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(AText) do
  begin
    if (I + 3 <= Length(AText)) and (AText[I] = '\') and (AText[I + 1] = 'x') then
    begin
      var HexVal := '$' + AText[I + 2] + AText[I + 3];
      Result := Result + Char(StrToInt(HexVal));
      Inc(I, 4);
    end
    else
    begin
      Result := Result + AText[I];
      Inc(I);
    end;
  end;
end;

type
  EEncodingError = class(Exception);

function Claude_EncodeToSingleLine_Fixed(const AText: string): string;
const
  MinPrintable = 32;
  MaxPrintable = 126;
var
  SB: TStringBuilder;
begin
  if AText = '' then
    Exit('');

  SB := TStringBuilder.Create(Length(AText) * 2);
  try
    for var I := Low(AText) to High(AText) do
    begin
      var Ch := AText[I];
      if (Ord(Ch) <= MaxPrintable) and (Ord(Ch) >= MinPrintable)
         and (Ch <> '\') then // Escape backslash too
        SB.Append(Ch)
      else
        SB.Append('\x').Append(IntToHex(Ord(Ch), 4));
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function Claude_DecodeFromSingleLine_Fixed(const AText: string): string;
var
  I: Integer;
  SB: TStringBuilder;
begin
  if AText = '' then
    Exit('');

  SB := TStringBuilder.Create(Length(AText));
  try
    I := Low(AText);
    while I <= High(AText) do
    begin
      if (I + 5 <= High(AText)) and (AText[I] = '\') and (AText[I + 1] = 'x') then
      begin
        try
          var HexStr := Copy(AText, I + 2, 4);
          var CharCode := StrToInt('$' + HexStr);
          SB.Append(Char(CharCode));
          Inc(I, 6);
        except
          on E: Exception do
            raise EEncodingError.CreateFmt('Invalid hex sequence at position %d: %s',
              [I, E.Message]);
        end;
      end
      else
      begin
        SB.Append(AText[I]);
        Inc(I);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function CoPilot_EncodeNonPrintableChars(const Input: string): string;
var
  i: Integer;
  ch: Char;
begin
  Result := '';
  for i := 1 to Length(Input) do
  begin
    ch := Input[i];
    if (Ord(ch) < 32) or (Ord(ch) > 126) then
      Result := Result + '#' + IntToStr(Ord(ch)) + ';'  // Replace non-printable characters
    else
      Result := Result + ch;  // Keep printable characters as is
  end;
end;

function CoPilot_EncodeNonPrintableChars_Fixed(const Input: string): string;
var
  i: Integer;
  ch: Char;
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    for i := 1 to Length(Input) do
    begin
      ch := Input[i];
      if ch = '#' then
        sb.Append('##')  // Escape the '#' character
      else if (Ord(ch) < 32) or (Ord(ch) > 126) then
        sb.AppendFormat('#%d;', [Ord(ch)])  // Replace non-printable characters
      else
        sb.Append(ch);  // Keep printable characters as is
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function CoPilot_DecodeNonPrintableChars_Fixed(const Input: string): string;

var
  i: Integer;
  sb: TStringBuilder;
  code: Integer;
  codeStr: string;
begin
  sb := TStringBuilder.Create;
  try
    i := 1;
    while i <= Length(Input) do
    begin
      if (Input[i] = '#') then
      begin
        if (i < Length(Input)) and (Input[i + 1] = '#') then
        begin
          sb.Append('#');  // Decode '##' to '#'
          Inc(i, 2);
        end
        else
        begin
          Inc(i);
          codeStr := '';
          while (i <= Length(Input)) and (Input[i] <> ';') do
          begin
            codeStr := codeStr + Input[i];
            Inc(i);
          end;
          if (i <= Length(Input)) and (Input[i] = ';') then
          begin
            if TryStrToInt(codeStr, code) then
              sb.Append(Char(code));
          end;
          Inc(i);
        end;
      end
      else
      begin
        sb.Append(Input[i]);
        Inc(i);
      end;
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function OpenAI_MakePrintable(const S: string): string;
var
  i: Integer;
  ch: Char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    ch := S[i];
    case Ord(ch) of
      9: Result := Result + '\t';  // Tab
      10: Result := Result + '\n'; // Line Feed
      13: Result := Result + '\r'; // Carriage Return
      32..126: Result := Result + ch; // Printable ASCII
    else
      // For other non-printable characters, use hexadecimal escape
      Result := Result + Format('\x%.2X', [Ord(ch)]);
    end;
  end;
end;

function OpenAI_MakePrintableUnicode_Fixed(const S: string): string;
var
  i: Integer;
  Code: Integer;
  SB: TStringBuilder;
  HighSurrogate, LowSurrogate: Char;
  CombinedCode: UInt32;
begin
  SB := TStringBuilder.Create(Length(S));
  try
    i := 1;
    while i <= Length(S) do
    begin
      Code := Ord(S[i]);
      if (Code >= $D800) and (Code <= $DBFF) then // High Surrogate
      begin
        if (i + 1) <= Length(S) then
        begin
          LowSurrogate := S[i + 1];
          if (Ord(LowSurrogate) >= $DC00) and (Ord(LowSurrogate) <= $DFFF) then
          begin
            // Combine surrogate pair into a single code point
            CombinedCode := ((Code - $D800) shl 10) + (Ord(LowSurrogate) - $DC00) + $10000;
            SB.AppendFormat('\U%.8X', [CombinedCode]);
            Inc(i, 2);
            Continue;
          end;
        end;
        // Invalid surrogate pair
        SB.AppendFormat('\u%.4X', [Code]);
      end
      else if (Code >= $DC00) and (Code <= $DFFF) then // Low Surrogate without preceding High Surrogate
      begin
        // Invalid surrogate
        SB.AppendFormat('\u%.4X', [Code]);
      end
      else
      begin
        case Code of
          Ord('\'): SB.Append('\\');            // Escape backslash
          9: SB.Append('\t');                    // Tab
          10: SB.Append('\n');                   // Line Feed
          13: SB.Append('\r');                   // Carriage Return
          //32..126:                             // *** fix below
          32..91, 93..126:
            SB.Append(S[i]);                     // Printable ASCII
        else
          if Code <= $FFFF then
            SB.AppendFormat('\u%.4X', [Code])    // Unicode \uXXXX
          else
            SB.AppendFormat('\U%.8X', [Code]);   // Unicode \UXXXXXXXX
        end;
      end;
      Inc(i);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function CodePointToUTF16(CodePoint: UInt32): string;
var
  HighSurrogate, LowSurrogate: Word;
begin
  if CodePoint <= $FFFF then
  begin
    // Basic Multilingual Plane (BMP)
    Result := Char(CodePoint);
  end
  else if CodePoint <= $10FFFF then
  begin
    // Characters beyond BMP - require surrogate pairs
    CodePoint := CodePoint - $10000;
    HighSurrogate := ($D800) + ((CodePoint shr 10) and $3FF);
    LowSurrogate := ($DC00) + (CodePoint and $3FF);
    Result := Char(HighSurrogate) + Char(LowSurrogate);
  end
  else
  begin
    // Invalid Code Point
    raise Exception.CreateFmt('Invalid Unicode code point: %d', [CodePoint]);
  end;
end;

function OpenAI_UnmakePrintableUnicode_Fixed(const S: string): string;
var
  i: Integer;
  LengthS: Integer;
  CurrentChar: Char;
  NextChar: Char;
  HexStr: string;
  CodePoint: UInt32;
  EscapeSeq: string;
begin
  Result := '';
  LengthS := Length(S);
  i := 1;
  while i <= LengthS do
  begin
    CurrentChar := S[i];
    if CurrentChar = '\' then
    begin
      if i = LengthS then
      begin
        // Trailing backslash without escape character
        raise Exception.Create('Invalid escape sequence at end of string.');
      end;

      NextChar := S[i + 1];
      case NextChar of
        '\':
          begin
            Result := Result + '\';
            Inc(i, 2);
          end;
        'n':
          begin
            Result := Result + #10; // Line Feed
            Inc(i, 2);
          end;
        'r':
          begin
            Result := Result + #13; // Carriage Return
            Inc(i, 2);
          end;
        't':
          begin
            Result := Result + #9; // Tab
            Inc(i, 2);
          end;
        'u':
          begin
            // Unicode escape sequence \uXXXX
            if (i + 5) > LengthS then
              raise Exception.CreateFmt('Incomplete \u escape sequence at position %d.', [i]);

            HexStr := Copy(S, i + 2, 4);
            // *** removed non-working code
//            if not TCharacter.IsHexDigit(HexStr[1]) or
//               not TCharacter.IsHexDigit(HexStr[2]) or
//               not TCharacter.IsHexDigit(HexStr[3]) or
//               not TCharacter.IsHexDigit(HexStr[4]) then
//              raise Exception.CreateFmt('Invalid \u escape sequence at position %d.', [i]);

            CodePoint := StrToInt('$' + HexStr);
            Result := Result + CodePointToUTF16(CodePoint);
            Inc(i, 6); // Skip past \uXXXX
          end;
        'U':
          begin
            // Unicode escape sequence \UXXXXXXXX
            if (i + 9) > LengthS then
              raise Exception.CreateFmt('Incomplete \U escape sequence at position %d.', [i]);

            HexStr := Copy(S, i + 2, 8);
            // Validate all 8 characters are hex digits
            // *** removed non-working code
//            if not TCharacter.IsHexDigit(HexStr[1]) or
//               not TCharacter.IsHexDigit(HexStr[2]) or
//               not TCharacter.IsHexDigit(HexStr[3]) or
//               not TCharacter.IsHexDigit(HexStr[4]) or
//               not TCharacter.IsHexDigit(HexStr[5]) or
//               not TCharacter.IsHexDigit(HexStr[6]) or
//               not TCharacter.IsHexDigit(HexStr[7]) or
//               not TCharacter.IsHexDigit(HexStr[8]) then
//              raise Exception.CreateFmt('Invalid \U escape sequence at position %d.', [i]);

            CodePoint := StrToInt('$' + HexStr);
            Result := Result + CodePointToUTF16(CodePoint);
            Inc(i, 10); // Skip past \UXXXXXXXX
          end;
      else
        begin
          // Unrecognized escape sequence, treat '\' + NextChar as literal
          Result := Result + NextChar;
          Inc(i, 2);
        end;
      end;
    end
    else
    begin
      // Regular character, append as-is
      Result := Result + CurrentChar;
      Inc(i);
    end;
  end;
end;

procedure TForm14.btnClaudeClick(Sender: TObject);
begin
  outEncoded.Text := Claude_EncodeToSingleLine(inpText.Text);
  outDecoded.Text := Claude_DecodeFromSingleLine(outEncoded.Text);
end;

procedure TForm14.btnClaudeFixedClick(Sender: TObject);
begin
  outEncoded.Text := Claude_EncodeToSingleLine_Fixed(inpText.Text);
  outDecoded.Text := Claude_DecodeFromSingleLine_Fixed(outEncoded.Text);
end;

procedure TForm14.btnCoPilotClick(Sender: TObject);
begin
  outEncoded.Text := CoPilot_EncodeNonPrintableChars(inpText.Text);
  outDecoded.Text := 'not implemented';
end;

procedure TForm14.btnCoPilotFixedClick(Sender: TObject);
begin
  outEncoded.Text := CoPilot_EncodeNonPrintableChars_Fixed(inpText.Text);
  outDecoded.Text := CoPilot_DecodeNonPrintableChars_Fixed(outEncoded.Text);
end;

procedure TForm14.btnGeminiClick(Sender: TObject);
begin
  outEncoded.Text := Gemini_EncodeNonPrintableChars(inpText.Text);
  outDecoded.Text := Gemini_DecodeNonPrintableChars(outEncoded.Text);
end;

procedure TForm14.btnGeminiFixedClick(Sender: TObject);
begin
  outEncoded.Text := Gemini_EncodeNonPrintableChars_Fixed(inpText.Text);
  outDecoded.Text := Gemini_DecodeNonPrintableChars_Fixed(outEncoded.Text);
end;

procedure TForm14.btnOpenAIClick(Sender: TObject);
begin
  outEncoded.Text := OpenAI_MakePrintable(inpText.Text);
  outDecoded.Text := 'not implemented';
end;

procedure TForm14.btnOpenAIFixedClick(Sender: TObject);
begin
  outEncoded.Text := OpenAI_MakePrintableUnicode_Fixed(inpText.Text);
  outDecoded.Text := OpenAI_UnmakePrintableUnicode_Fixed(outEncoded.Text);
end;

end.
