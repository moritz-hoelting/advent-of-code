{$mode objfpc}

program part_1;

uses
  Classes, Math, StrUtils, SysUtils;

var
  fs: TFileStream;
  s: AnsiString;
  parts, subparts: TStringArray;
  i: Integer;
  sum, val: Int64;

function calculate(a, b: Int64): Int64;
var
    res, i, l10, half, divisor, upper, lower: Int64;
begin
    res := 0;
    for i := a to b do
    begin
        l10 := Length(IntToStr(i));
        half := l10 div 2;
        divisor := Trunc(Power(10, half));
        upper := i div divisor;
        lower := i mod divisor;

        if upper = lower then
            res := res + i;
    end;

    calculate := res;
end;

begin
    fs := TFileStream.Create('input.txt', fmOpenRead);
    try
        SetLength(s, fs.Size);
        fs.ReadBuffer(s[1], fs.Size);
    finally
        fs.Free;
    end;

    sum := 0;

    parts := SplitString(s, ',');

    for i := 0 to High(parts) do
    begin
        subparts := SplitString(parts[i], '-');

        val := calculate(StrToInt64(subparts[0]), StrToInt64(subparts[1]));
        sum := sum + val;

    end;
    WriteLn('Part 1 Solution: ', IntToStr(sum));
end.