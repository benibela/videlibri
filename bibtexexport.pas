unit bibtexexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, Buttons;

type

  { TBibTexExportFrm }

  TBibTexExportFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    FileNameEdit1: TFileNameEdit;
    GroupBox1: TGroupBox;
    clipboardExport: TRadioButton;
    fileExport: TRadioButton;
    exportWhich: TRadioGroup;
    RadioGroup1: TRadioGroup;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  BibTexExportFrm: TBibTexExportFrm;

implementation
uses bbutils, applicationconfig, bookWatchMain,booklistreader,Clipbrd,math;
{ TBibTexExportFrm }

procedure TBibTexExportFrm.RadioGroup2Click(Sender: TObject);
begin

end;


procedure TBibTexExportFrm.BitBtn1Click(Sender: TObject);
  const charToReplace:array[1..7] of record
    c:array[1..5] of string;
    r:array[1..5] of string;
    r2:array[1..5] of string;
  end = ((c:(    'á'  ,   'é',     'í',     'ó',     'ú'); //utf8 original
          r:('{\''a}','{\''e}','{\''i}','{\''o}','{\''u}');//latex ersetzung
          r2:(   'a',     'e',     'i',     'o',     'u')),//alternative

         (c:(   'à',    'è',    'ì',    'ò',    'ù');
          r:('{\`a}','{\`e}','{\`i}','{\`o}','{\`u}');
          r2:(  'a',    'e',    'i',    'o',    'u')),

         (c:(   'â',    'ê',    'î',    'ô',    'û');
          r:('{\^a}','{\^e}','{\^i}','{\^o}','{\^u}');
          r2:(  'a',    'e',    'i',    'o',    'u')),

         (c:(   'ä',    'ö',    'ü',   'ø',   'Ø');
          r:('{\"a}','{\"o}','{\"u}','{\o}','{\O}');
          r2:(  'ae',   'oe',   'ue',  'o',   'O')),

         (c:(   'œ',    'Œ',    'æ',    'Æ',  '¡');
          r:('{\ce}','{\CE}','{\ae}','{\AE}','{!`}');
          r2:(  'ce'   ,'CE',   'ae',   'AE', '!')),

         (c:(  'ß',         '™',             '©',               '®',            '•');
          r:('{\ss}', '{\texttrademark}','{\copyright}','{\textregistered}','{\textbullet}');
          r2:( 'ss',        'TM',           '(c)',             '(R)',           '.')),

         (c:(  '§',    '†',    '‡',             '·',              '¿');
          r:('{\S}','{\dag}','{\ddag}','{\textperiodcentered}','{?`}');
          r2:( 'S',    '.',    '.',             '.',              '.')));

var outputEncoding:longint;
  //Konvertiert UTF-8 zum Ausgabezeichensatz
  function convStr(s:string):string;
    var i,j:longint;
  begin
    s:=StringReplace(s,'\','\\',[rfReplaceAll]);
    s:=StringReplace(s,'{','\{',[rfReplaceAll]);
    s:=StringReplace(s,'"','"',[rfReplaceAll]);
    result:=s;
    case outputEncoding of
      0: begin //UTF-8 to Latex
        //Result:=Utf8ToAnsi(result);
        //TODO: Optimize
        for i:=low(charToReplace) to high(charToReplace) do
          for j:=low(charToReplace[i].c) to high(charToReplace[i].c) do
            result:=StringReplace(result,charToReplace[i].c[j],charToReplace[i].r[j],[rfReplaceAll]);

      end;
      //1: Result:=Utf8ToAnsi(result);
      //else result:=s;
    end;
  end;

  function removeBadIDChar(s:string):string; //=>ascii
  var i,j:longint;
  begin
    result:=StringReplace(s,' ','_',[rfReplaceAll]);
    result:=StringReplace(result,',','_',[rfReplaceAll]);
    for i:=low(charToReplace) to high(charToReplace) do
      for j:=low(charToReplace[i].c) to high(charToReplace[i].c) do
        result:=StringReplace(result,charToReplace[i].c[j],charToReplace[i].r2[j],[rfReplaceAll]);
  end;

const AUTHOR_LEN=5;TITLE_LEN=5;
var all:boolean;
    exportStr:string;
    book: TBook;
    id,authorId,titleID:string;
    i:LONGINt;
begin
  outputEncoding:=RadioGroup1.ItemIndex;

  all:=exportWhich.ItemIndex=0;
  exportStr:='';
  for i:=0 to mainForm.BookList.Items.count-1 do
    if all or (mainForm.BookList.Items[i].Selected) then begin
      book:=TBook(mainForm.BookList.Items[i].Tag);
      if book=nil then continue;

      //autor-jahr, autor-titel, titel-jahr, autor, titel, id
      if book.author<>'' then begin
        if pos(',',book.author)>0 then  //nachname, vorname
          authorId:=copy(removeBadIDChar(book.author),1,min(pos(',',book.author)-1,AUTHOR_LEN))
         else if pos(' ',book.author)=0 then  //nachname
          authorId:=copy(removeBadIDChar(book.author),1,AUTHOR_LEN)
         else
          authorId:=copy(removeBadIDChar(book.author),length(book.author)-AUTHOR_LEN+1,AUTHOR_LEN);
        if book.year<>'' then
          id:=authorId+book.year
         else if book.title<>'' then
          id:=authorId+'_'+copy(removeBadIDChar(book.title),1,TITLE_LEN)
         else id:=authorId;
      end else if (book.title<>'') and (book.year<>'') then
        id:=copy(removeBadIDChar(book.title),1,TITLE_LEN)+book.year
       else if book.title<>'' then
        id:=copy(removeBadIDChar(book.title),1,TITLE_LEN+AUTHOR_LEN)
       else if book.id[1] in ['0'..'9'] then
        id:=removeBadIDChar(book.id)
       else
        id:='b'+removeBadIDChar(book.id);

      exportStr+=#13#10'@book{'+convStr(id)+','#13#10;
      titleID:=convStr(book.title);
      if book.author<>'' then exportStr+='  author = "'+convStr(book.author)+'",'#13#10;
      if book.title<>'' then exportStr+='  title = "'+convStr(book.title)+'",'#13#10;
      if book.year<>'' then exportStr+='  year = "'+convStr(book.year)+'",'#13#10;
      exportStr+='}'#13#10;
    end;
    if ExtractFileExt(FileNameEdit1.Text)='' then
      FileNameEdit1.Text:=FileNameEdit1.Text+'.bib';

    if clipboardExport.Checked then Clipboard.AsText:=exportStr
    else case outputEncoding of
      0: saveFileFromStr(FileNameEdit1.Text,exportStr); //exportStr is ASCII, no convert
      1: saveFileFromStr(FileNameEdit1.Text,Utf8ToAnsi(exportStr)); //exportStr is UTF-8, convert to ANSI
      2: saveFileFromStr(FileNameEdit1.Text,exportStr); //exportStr is UTF-8, no convert
    end;
end;

procedure TBibTexExportFrm.FormCreate(Sender: TObject);
begin

end;

procedure TBibTexExportFrm.FormShow(Sender: TObject);
begin
  exportWhich.Controls[1].Enabled:=mainForm.BookList.SelCount<>0;
end;


initialization
  {$I bibtexexport.lrs}

end.

