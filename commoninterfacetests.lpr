program commoninterfacetests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, commoninterface, commontestutils
  { you can add units after this };

var fi, fi2: TFormInput;
  fp, fp2: TFormParams;
  fs, fs2: TFormSelect;
  nc: TNotificationConfig;
begin
  writeln('Test auto generated JSON serialization');
  writeln();
  fi := TFormInput.Create;
  fi.name := 'foo';
  fi.caption := 'labeling';
  writeln(fi.toJSON());
  test(fi.toJSON(), '{"name": "foo", "caption": "labeling", "value": ""}');

  fi2 := TFormInput.fromJSON(fi.toJSON());
  writeln(fi2.name);
  test(fi.name, fi2.name);
  writeln(fi2.caption);
  test(fi.caption, fi2.caption);
  writeln(fi2.toJSON());
  test(fi2.toJSON(), '{"name": "foo", "caption": "labeling", "value": ""}');

  writeln;

  fs := TFormSelect.Create;
  fs.name := 'fs';
  fs.caption := 'kirk';
  writeln(fs.toJSON());
  test(fs.toJSON(), '{"name": "fs", "caption": "kirk", "value": "", "optionCaptions": [], "optionValues": []}');
  setlength(fs.optionCaptions, 3);
  setlength(fs.optionValues, 3);
  fs.optionCaptions[0] := 'N1';
  fs.optionValues[0] := 'V1';
  fs.optionCaptions[1] := 'Na2';
  fs.optionValues[1] := 'Va2';
  fs.optionCaptions[2] := 'Na3';
  fs.optionValues[2] := 'Va3';
  writeln(fs.toJSON());
  test(fs.toJSON(), '{"name": "fs", "caption": "kirk", "value": "", "optionCaptions": ["N1", "Na2", "Na3"], "optionValues": ["V1", "Va2", "Va3"]}');
  fs2 := TFormSelect.fromJSON(fs.toJSON());
  writeln(fs2.toJSON());
  test(fs2.toJSON(), '{"name": "fs", "caption": "kirk", "value": "", "optionCaptions": ["N1", "Na2", "Na3"], "optionValues": ["V1", "Va2", "Va3"]}');
  fs2.free;
  writeln;


  fp := TFormParams.Create;
  SetLength(fp.inputs, 1);
  fp.inputs[0] := fi;
  writeln(fp.toJSON());
  test(fp.toJSON(), '{"inputs": ["FormInput", {"name": "foo", "caption": "labeling", "value": ""}]}');
  SetLength(fp.inputs, 2);
  fp.inputs[1] := fi2;
  writeln(fp.toJSON());
  test(fp.toJSON(), '{"inputs": ["FormInput", {"name": "foo", "caption": "labeling", "value": ""}, "FormInput", {"name": "foo", "caption": "labeling", "value": ""}]}');
  SetLength(fp.inputs, 3);
  fp.inputs[2] := fs;
  writeln(fp.toJSON());
  test(fp.toJSON(), '{"inputs": ["FormInput", {"name": "foo", "caption": "labeling", "value": ""}, "FormInput", {"name": "foo", "caption": "labeling", "value": ""}, "FormSelect", {"name": "fs", "caption": "kirk", "value": "", "optionCaptions": ["N1", "Na2", "Na3"], "optionValues": ["V1", "Va2", "Va3"]}]}');

  fp2 := TFormParams.fromJSON(fp.toJSON());
  writeln(fp2.toJSON());
  test(fp2.toJSON(), '{"inputs": ["FormInput", {"name": "foo", "caption": "labeling", "value": ""}, "FormInput", {"name": "foo", "caption": "labeling", "value": ""}, "FormSelect", {"name": "fs", "caption": "kirk", "value": "", "optionCaptions": ["N1", "Na2", "Na3"], "optionValues": ["V1", "Va2", "Va3"]}]}');
  writeln;

  fp2.free;
  fp.free;

  nc := TNotificationConfig.fromJSON('{"lastTime": 123}');
  test(nc.lastTime, 123);
  test(nc.toJSON(), '{"enabled": false, "serviceDelay": 0, "lastTime": 123, "lastTitle": "", "lastText": ""}');
  nc.free;
  nc := TNotificationConfig.fromJSON('{"lastTime": 1599549012000}');
  test(nc.lastTime, 1599549012000);
  test(nc.toJSON(), '{"enabled": false, "serviceDelay": 0, "lastTime": 1599549012000, "lastTitle": "", "lastText": ""}');
  nc.free;
  nc := TNotificationConfig.fromJSON('{"lastTime": 9223372036854775807}');
  test(nc.lastTime, 9223372036854775807);
  test(nc.toJSON(), '{"enabled": false, "serviceDelay": 0, "lastTime": 9223372036854775807, "lastTitle": "", "lastText": ""}');
  nc.free;
  nc := TNotificationConfig.fromJSON('{"lastTime": -9223372036854775808}');
  test(nc.lastTime, -9223372036854775808);
  test(nc.toJSON(), '{"enabled": false, "serviceDelay": 0, "lastTime": -9223372036854775808, "lastTitle": "", "lastText": ""}');
  nc.free;
  nc := TNotificationConfig.fromJSON('{"lastTime": "9223372036854775807"}');
  test(nc.lastTime, 9223372036854775807);
  test(nc.toJSON(), '{"enabled": false, "serviceDelay": 0, "lastTime": 9223372036854775807, "lastTitle": "", "lastText": ""}');
  nc.free;
  nc := TNotificationConfig.fromJSON('{"lastTime": "-9223372036854775808"}');
  test(nc.lastTime, -9223372036854775808);
  test(nc.toJSON(), '{"enabled": false, "serviceDelay": 0, "lastTime": -9223372036854775808, "lastTitle": "", "lastText": ""}');
  nc.free;

end.

