unit bibtexexport;

{$I videlibrilanguageconfig.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, Buttons, applicationformconfig;

type

  { TBibTexExportFrm }

  TBibTexExportFrm = class(TVideLibriForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBoxShowEncoding: TCheckBox;
    ComboBoxIdFirst: TComboBox;
    ComboBoxIdSecond: TComboBox;
    ComboBoxIdThird: TComboBox;
    FileNameEdit1: TFileNameEdit;
    GroupBox1: TGroupBox;
    clipboardExport: TRadioButton;
    fileExport: TRadioButton;
    exportWhich: TRadioGroup;
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBoxShowEncodingChange(Sender: TObject);
    procedure ComboBoxIdThirdChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { private declarations }
    procedure mySize;
  public
    { public declarations }
  end; 

var
  BibTexExportFrm: TBibTexExportFrm;

implementation
uses bbutils, bbutilsbeta, applicationconfig, bookWatchMain,booklistreader,Clipbrd, xquery__serialization_nodes,xquery.internals.common;
{ TBibTexExportFrm }

type TBibTeXIDFormats = (
  bibtexIdReservedA, bibtexIdReservedB,
  bibtexIdID = 2,
  bibtexIdLastnameYear,
  bibtexIdLastnameTitle,
  bibtexIdLastnameTitlestart,
  bibtexIdAuthorYear,
  bibtexIdAuthorTitle,
  bibtexIdAuthorTitlestart,
  bibtexIdTitleYear,
  bibtexIdTitlestartYear,
  bibtexIdLastname,
  bibtexIdAuthor,
  bibtexIdTitle,
  bibtexIdTitlestart
);

type TBibTeXIDFormat = record
  hasAuthor, hasTitle, hasID, hasYear: boolean;
  shortenAuthor, shortenTitle: boolean;
  class function getLastName(author: string): string; static;
  class function getShortTitle(title: string): string; static;
  function makeID(book: TBook; out finalID: string): boolean;
  procedure initFromEnum(enum: TBibTeXIDFormats);
end;


class function TBibTeXIDFormat.getLastName(author: string): string;
var
  tempview: TPCharView;
begin
  tempview := author.pcharView;
  tempview.cutBeforeFind(';');
  tempview.cutBeforeFind('[');
  if tempview.count(',') = 1 then begin
    tempview.cutBeforeFind(','); //assume Last Name, First Name
  end else begin
    //assume First Name Last Name (=0)
    //OR   First Name Last Name 1, First Name Last Name 2, ... (> 1)
    //in second case take first author
    tempview.cutBeforeFind(',');
    if tempview.contains('.') then begin
      tempview.moveAfterFindLast('.');
    end else begin
      //multiple first names or multiple last names???
      //assume former
      tempview.trim();
      tempview.moveAfterFindLast(' ');
    end;
  end;
  tempview.trim();
  result := tempview.ToString;
end;

class function TBibTeXIDFormat.getShortTitle(title: string): string;
var
  tempview: TPCharView;
  firstSpace, secondSpace: pchar;
begin
  tempview := title.pcharView;
  tempview.trim();
  if tempview.beginsWith('¬') then begin
    tempview.moveBy(length('¬'));
    if tempview.contains('¬') then tempview.moveAfterFind('¬');
    tempview.trim();
  end;
  firstSpace := tempview.find(' ');
  if firstSpace <> nil then begin
    if firstSpace - tempview.data <= 5 then begin
      secondSpace := tempview.viewAfter(firstSpace).find(' ');
      if secondSpace <> nil then firstSpace := secondSpace;
    end;
    tempview.cutBeforeFind(firstSpace);
  end;
  result := tempview.ToString;
end;

function TBibTeXIDFormat.makeID(book: TBook; out finalID: string): boolean;
var
  temp, tempID: String;
  builder: TStrBuilder;
  c: Char;
  lastWasSpace: Boolean;
begin
  if hasAuthor and (book.author = '') then exit(false);
  if hasTitle and (book.title = '') then exit(false);
  if hasID and (book.id = '') then exit(false);
  if hasYear and (book.year = '') then exit(false);

  tempID := '';
  if hasAuthor then begin
    if shortenAuthor then temp := getLastName(book.author)
    else temp := book.author;
    tempID += temp;
  end;

  if hasTitle then begin
    if shortenTitle then temp := getShortTitle(book.title)
    else temp := book.title;
    if temp <> '' then temp[1] := UpCase(temp[1]);
    tempID += temp;
  end;

  if hasID then
    tempID += book.id;

  if hasYear then
    tempID += book.year;

  if IsValidIdent(tempID) then finalID := tempID //Pascal Ident is close enough
  else begin
    //see https://tex.stackexchange.com/questions/408530/what-characters-are-allowed-to-use-as-delimiters-for-bibtex-keys
    tempID := normalizeString(tempID, unfNFKD);
    lastWasSpace := false;
    builder.init(@finalID, length(tempID));
    for c in tempID do begin
      case c of
        '*', '+', '-', '.', '/', ':', ';', '?', '@', 'A'..'Z', '_', 'a'..'z', '0'..'9': begin
          if not lastWasSpace then builder.append(c)
          else begin
            builder.append(upcase(c));
            lastWasSpace := false;
          end;
        end;
        ' ': lastWasSpace := true;
      end;
    end;
    builder.final;
  end;
  result := finalID <> '';
end;

procedure TBibTeXIDFormat.initFromEnum(enum: TBibTeXIDFormats);
begin
  hasAuthor := enum in [bibtexIdLastnameYear, bibtexIdLastnameTitle, bibtexIdLastnameTitlestart,
                        bibtexIdAuthorYear, bibtexIdAuthorTitle, bibtexIdAuthorTitlestart,
                        bibtexIdLastname, bibtexIdAuthor];

  hasTitle := enum in [
    bibtexIdLastnameTitle, bibtexIdLastnameTitlestart,
    bibtexIdAuthorTitle, bibtexIdAuthorTitlestart,
    bibtexIdTitleYear, bibtexIdTitlestartYear,
    bibtexIdTitle, bibtexIdTitlestart];

  hasId := enum in [bibtexIdID];

  hasYear := enum in [
    bibtexIdLastnameYear,
    bibtexIdAuthorYear,
    bibtexIdTitleYear,
    bibtexIdTitlestartYear
  ];

  shortenAuthor := enum in [
    bibtexIdLastnameYear, bibtexIdLastnameTitle,  bibtexIdLastnameTitlestart,
    bibtexIdLastname
  ];

  shortenTitle := enum in [
    bibtexIdLastnameTitlestart,
    bibtexIdAuthorTitlestart,
    bibtexIdTitlestartYear,
    bibtexIdTitlestart];

end;

procedure TBibTexExportFrm.RadioGroup2Click(Sender: TObject);
begin

end;

procedure TBibTexExportFrm.mySize;
begin
  //my auto size height calculation
  //AutoSize := true or AdjustSize does not work Lazarus 2.0.10 r64689M FPC 3.2.2 x86_64-linux-gtk2
  if CheckBoxShowEncoding.Checked then Height := CheckBoxShowEncoding.Top + CheckBoxShowEncoding.Height + 15 + RadioGroup1.Height + panel1.Height + GroupBox1.Height
  else Height := CheckBoxShowEncoding.Top + CheckBoxShowEncoding.Height + 10 + panel1.Height + GroupBox1.Height
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
    s:=StringReplace(s,'"','',[rfReplaceAll]);
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

var all:boolean;
    exportStr, id:string;
    book: TBook;
    ids: array[1..6] of TBibTeXIDFormat;

    i, idCounter:LONGINt;
    builder: TStrBuilder;
    duplicateIdCheck: TXQHashsetStr;
begin
  if CheckBoxShowEncoding.Checked then outputEncoding:=RadioGroup1.ItemIndex
  else outputEncoding:=2;//utf-8

  for i := 1 to high(ids) do ids[i] := default(TBibTeXIDFormat); //this line is completely useless but necessary to hide a warning
  ids[1].initFromEnum(TBibTeXIDFormats(ComboBoxIdFirst.ItemIndex + 2));
  ids[2].initFromEnum(TBibTeXIDFormats(ComboBoxIdSecond.ItemIndex + 2));
  ids[3].initFromEnum(TBibTeXIDFormats(ComboBoxIdThird.ItemIndex + 2));
  ids[4].initFromEnum(bibtexIdTitle);
  ids[5].initFromEnum(bibtexIdAuthor);
  ids[6].initFromEnum(bibtexIdID);
  all:=exportWhich.ItemIndex=0;
  exportStr:='';
  duplicateIdCheck.init;
  builder.init(@exportStr);
  for i:=0 to mainForm.BookList.Items.count-1 do
    if all or (mainForm.BookList.Items[i].Selected) then begin
      book:=TBook(mainForm.BookList.Items[i].data.obj);
      if book=nil then continue;

      if not ids[1].makeID(book, id) and not ids[2].makeID(book, id) and not ids[3].makeID(book, id)
         and not ids[4].makeID(book, id) and not ids[5].makeID(book, id) and not ids[6].makeID(book, id)  then begin
        id := 'book';
      end;
      if duplicateIdCheck.contains(id) then begin
        id += '_';
        idCounter := 2;
        while duplicateIdCheck.contains(id + inttostr(idCounter)) do inc(idCounter);
        id := id + inttostr(idCounter);
      end;
      duplicateIdCheck.include(id);


      builder.append(#13#10'@book{'+id+','#13#10);
      if book.author<>'' then builder.append('  author = "'+convStr(book.author)+'",'#13#10);
      if book.title<>'' then builder.append('  title = "'+convStr(book.title)+'",'#13#10);
      if book.year<>'' then builder.append('  year = "'+convStr(book.year)+'",'#13#10);
      builder.append('}'#13#10);
    end;
    builder.final;
    if ExtractFileExt(FileNameEdit1.Text)='' then
      FileNameEdit1.Text:=FileNameEdit1.Text+'.bib';

    if clipboardExport.Checked then Clipboard.AsText:=exportStr
    else case outputEncoding of
      0: strSaveToFile(FileNameEdit1.Text,exportStr); //exportStr is ASCII, no convert
      1: strSaveToFile(FileNameEdit1.Text,strConvertFromUtf8(exportStr,CP_LATIN1)); //exportStr is UTF-8, convert to ANSI
      2: strSaveToFile(FileNameEdit1.Text,exportStr); //exportStr is UTF-8, no convert
    end;

    userConfig.WriteInteger('BibTeX-Export', 'Which', exportWhich.ItemIndex);
    userConfig.WriteBool('BibTeX-Export', 'Clipboard', clipboardExport.Checked);
    userConfig.WriteString('BibTeX-Export', 'Filename', FileNameEdit1.Text);
    userConfig.WriteInteger('BibTeX-Export', 'Charset', RadioGroup1.ItemIndex);
    userConfig.WriteInteger('BibTeX-Export', 'ID-1', ComboBoxIdFirst.ItemIndex + 2);
    userConfig.WriteInteger('BibTeX-Export', 'ID-2', ComboBoxIdSecond.ItemIndex + 2);
    userConfig.WriteInteger('BibTeX-Export', 'ID-3', ComboBoxIdThird.ItemIndex + 2);
    duplicateIdCheck.done;
end;

procedure TBibTexExportFrm.CheckBoxShowEncodingChange(Sender: TObject);
begin
  if RadioGroup1.visible = CheckBoxShowEncoding.Checked then exit;
  //AutoSize := false;
  RadioGroup1.visible := CheckBoxShowEncoding.Checked;
  mySize;
end;

procedure TBibTexExportFrm.ComboBoxIdThirdChange(Sender: TObject);
begin

end;

procedure TBibTexExportFrm.FormCreate(Sender: TObject);
begin
end;

procedure TBibTexExportFrm.FormShow(Sender: TObject);
begin
  exportWhich.ItemIndex := userConfig.ReadInteger('BibTeX-Export', 'Which', 1);
  clipboardExport.Checked := userConfig.ReadBool('BibTeX-Export', 'Clipboard', false);
  FileNameEdit1.Text:= userConfig.ReadString('BibTeX-Export', 'Filename', '');
  RadioGroup1.ItemIndex := userConfig.ReadInteger('BibTeX-Export', 'Charset', 2);

  ComboBoxIdSecond.Items.Assign(ComboBoxIdFirst.Items);
  ComboBoxIdThird.Items.Assign(ComboBoxIdFirst.Items);
  ComboBoxIdFirst.ItemIndex := userConfig.ReadInteger('BibTeX-Export', 'ID-1', 3) - 2;
  ComboBoxIdSecond.ItemIndex := userConfig.ReadInteger('BibTeX-Export', 'ID-2', 4) - 2;
  ComboBoxIdThird.ItemIndex := userConfig.ReadInteger('BibTeX-Export', 'ID-3', 2) - 2;

  exportWhich.Controls[1].Enabled:=(mainForm.BookList.SelCount<>0);
  if not exportWhich.Controls[1].Enabled then
    exportWhich.ItemIndex:=0;
  mySize;
end;

procedure TBibTexExportFrm.Panel1Click(Sender: TObject);
begin

end;

{$if false}
procedure unitTests;
  procedure lastNameTest(const fullname, lastname: string);
  begin
    if TBibTeXIDFormat.getLastName(fullname) <> lastname then
      writeln(TBibTeXIDFormat.getLastName(fullname));
  end;
  procedure shortTitelTest(const fulltitle, shorttitle: string);
  begin
    if TBibTeXIDFormat.getShortTitle(fulltitle) <> shorttitle then
      writeln(TBibTeXIDFormat.getShortTitle(fulltitle), ' <> ', shorttitle);
  end;

begin
  writeln('Running tests');
  lastNameTest('Heinlein, Sylvia; Wiemers, Sabine', 'Heinlein');
  lastNameTest('Bernard, Jennifer ¬[VerfasserIn]', 'Bernard');
  lastNameTest('Kevin Lewis [Regisseur/in] ; Nicolas Cage [Schauspieler/in] ; Emily Tosta [Schauspieler/in] ; Ric Reitz [Schauspieler/in] ; G. O. Parsons [Drehbuchautor/in] ; David Newbert [Kamera]',
               'Lewis');
  lastNameTest('Gosch, Dietmar *1950-*', 'Gosch');
  lastNameTest('Paul J Deitel', 'Deitel');
  lastNameTest('David B. Coe', 'Coe');
  lastNameTest('Luk''janenko, Sergej V.', 'Luk''janenko');
  lastNameTest('X Y. A B', 'A B');

  shortTitelTest('¬The¬ Sinking City', 'Sinking City');
  shortTitelTest('Körperschaftsteuergesetz', 'Körperschaftsteuergesetz');
  shortTitelTest('Das Körperschaftsteuergesetz', 'Das Körperschaftsteuergesetz');
  shortTitelTest('Die', 'Die');
  shortTitelTest('Die      ', 'Die');
  shortTitelTest('Twenty letters to a friend', 'Twenty');
end;
{$endif}


initialization
  {$I bibtexexport.lrs}

//bibtexexport.unitTests;

end.


