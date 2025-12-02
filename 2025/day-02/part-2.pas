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
    res, i, l10, half, cmp, val: Int64;
    j, k: Integer;
    allEqual: Boolean;
begin
    res := 0;
    for i := a to b do
    begin
        l10 := Length(IntToStr(i));
        half := l10 div 2;
        for j := 1 to half do
        begin
            if (l10 mod j) <> 0 then
                continue;
            allEqual := True;
            cmp := (i div Trunc(Power(10, l10 - j))) mod Trunc(Power(10, j));
            for k := 1 to (l10 div j) - 1 do
            begin
                val := (i div Trunc(Power(10, l10 - j * (k + 1)))) mod Trunc(Power(10, j));
                if cmp <> val then
                begin
                    allEqual := False;
                    break;
                end;
            end;
            if allEqual then
            begin
                res := res + i;
                break;
            end;
        end;
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
    WriteLn('Part 2 Solution: ', IntToStr(sum));
end.