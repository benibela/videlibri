unit commoninterfacehelpers;

{$mode objfpc}{$H+}{$ModeSwitch advancedrecords}{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils, commoninterface;

type T_FormInputHelper = class helper for TFormInput
  function equals(other: TFormInput): boolean;
end;

implementation

function T_FormInputHelper.equals(other: TFormInput): boolean;
  function equalSelects(a,b: TFormSelect): boolean;
  var
    i: Integer;
  begin
    if length(a.optionCaptions) <> length(b.optionCaptions) then exit(false);
    if length(a.optionValues) <> length(b.optionValues) then exit(false);
    if length(a.optionCaptions) <> length(a.optionValues) then exit(false);
    for i := 0 to high(a.optionCaptions) do
      if (a.optionCaptions[i] <> b.optionCaptions[i]) or
         (a.optionValues[i] <> b.optionValues[i]) then exit(false);
    result := true;
  end;

begin
  if self = other then exit(true);
  if self.ClassType <> other.ClassType then exit(false);
  if    (name <> other.name)
     or (caption <> other.caption)
     or (value <> other.value) then exit(false);
  if self is TFormSelect then exit(equalSelects(TFormSelect(self), other as TFormSelect));
  result := true;
end;

end.

