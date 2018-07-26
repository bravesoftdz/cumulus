unit Utils;

interface

uses Classes, SysUtils;

function Element(element_number: integer;
                 const delimiter: String;
                 const source: String): String;
function URLDecode(const S: String): String;
function StrCount(Text : String; Token : Char) : Integer;
function Implode( glue : Char; pieces : TStringList) : String;
function Explode( separator : Char; const str : String; makelowercase : boolean = false) : TStringList; overload;
function Explode( separator : Char; const str : String; limit : Integer; makelowercase : boolean = false) : TStringList; overload;

implementation

function Element(element_number: integer;
                 const delimiter: String;
                 const source: String): String;
var
  lst: TStringList;
begin
  lst := TStringList.Create;
  try
    lst.Sorted := false;
    lst.Duplicates := dupIgnore;
    lst.LineBreak := delimiter;
    lst.Text := source;
    if element_number < lst.Count then begin
      result := lst.Strings[element_number];
    end else begin
      result := '';
    end;
  finally
    lst.Free;
  end;
end;

function URLDecode(const S: String): String;
// Author: www.delphidabbler.com
var
  Idx: Integer;   // loops thru chars in String
  Hex: String;    // String of hex characters
  Code: Integer;  // hex character code (-1 on error)
begin
  // Intialise result and String index
  Result := '';
  Idx := 1;
  // Loop thru String decoding each character
  while Idx <= Length(S) do
  begin
    case S[Idx] of
      '%':
      begin
        // % should be followed by two hex digits - exception otherwise
        if Idx <= Length(S) - 2 then
        begin
          // there are sufficient digits - try to decode hex digits
          Hex := S[Idx+1] + S[Idx+2];
          Code := SysUtils.StrToIntDef('$' + Hex, -1);
          Inc(Idx, 2);
        end
        else
          // insufficient digits - error
          Code := -1;
        // check for error and raise exception if found
        if Code = -1 then
          raise SysUtils.EConvertError.Create(
            'Invalid hex digit in URL'
          );
        // decoded OK - add character to result
        Result := Result + Chr(Code);
      end;
      '+':
        // + is decoded as a space
        Result := Result + ' '
      else
        // All other characters pass thru unchanged
        Result := Result + S[Idx];
    end;
    Inc(Idx);
  end;
end;

function StrCount(Text : String; Token : Char) : Integer;
var
    Count : Integer;
    I     : Integer;
begin
    Count := 0;
    For I := 1 to Length(Text) do
        If Text[I]=Token then Inc(Count);
    result := Count;
end;

function Implode( glue : Char; pieces : TStringList) : String;
var
    i : integer;
begin
    result := '';

    if pieces<>nil then begin
        if pieces.count<>0 then begin
            // Process stuff here
            result := pieces.strings[0];
            for i := 1 to pieces.count-1 do begin
                result := result + glue;
                result := result + pieces.Strings[i];
            end;
        end;
    end;
end;

function Explode( separator : Char; const str : String; makelowercase : boolean = false) : TStringList;
begin
    result := explode(separator, str, 0);
end;

function Explode( separator : Char; const str : String; limit : Integer; makelowercase : boolean = false) : TStringList;
var
  count : Integer;
  i     : Integer;
  temp  : TStringList;
  Token : String;
begin
  temp := TStringList.Create;
  try
    count := strCount(str, separator);

    for I := 0 to count do begin
      Token := trim(element(i,separator,str));
      If Length(Token)<>0 then begin
        if makelowercase then begin
          temp.Add(lowercase(Token));
        end else begin
          temp.Add(Token);
        end;
      end;
    end;
  finally
    result := temp;
  end;
end;

end.
