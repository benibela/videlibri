program commoninterfacetests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, commoninterface
  { you can add units after this };

var fi, fi2: TFormInput;
  fp, fp2: TFormParams;
  fs, fs2: TFormSelect;
begin
  fi := TFormInput.Create;
  fi.name := 'foo';
  fi.caption := 'labeling';
  writeln(fi.toJSON());

  fi2 := TFormInput.fromJSON(fi.toJSON());
  writeln(fi2.name);
  writeln(fi2.caption);
  writeln(fi2.toJSON());

  writeln;

  fs := TFormSelect.Create;
  fs.name := 'fs';
  fs.caption := 'kirk';
  writeln(fs.toJSON());
  setlength(fs.options, 3);
  fs.options[0].name := 'N1';
  fs.options[0].value := 'V1';
  fs.options[1].name := 'Na2';
  fs.options[1].value := 'Va2';
  fs.options[2].name := 'Na3';
  fs.options[2].value := 'Va3';
  writeln(fs.toJSON());
  fs2 := TFormSelect.fromJSON(fs.toJSON());
  writeln(fs2.toJSON());
  fs2.free;
  writeln;


  fp := TFormParams.Create;
  SetLength(fp.inputs, 1);
  fp.inputs[0] := fi;
  writeln(fp.toJSON());
  SetLength(fp.inputs, 2);
  fp.inputs[1] := fi2;
  writeln(fp.toJSON());
  SetLength(fp.inputs, 3);
  fp.inputs[2] := fs;
  writeln(fp.toJSON());

  fp2 := TFormParams.fromJSON(fp.toJSON());
  writeln(fp2.toJSON());

  writeln;

  fp2.free;
  fp.free;

end.

