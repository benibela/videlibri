unit bookSearchForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  CheckLst, StdCtrls,bookListView, ComCtrls, Menus,librarySearcher,booklistreader,TreeListView,math,
  librarySearcherAccess;

type

  { TbookSearchFrm }

  TbookSearchFrm = class(TForm)
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    LabelSaveTo: TLabel;
    saveToAccountMenu: TPopupMenu;
    searchAuthorHint: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    searchKeywords: TComboBox;
    searchTitleHint: TLabel;
    searchYear: TComboBox;
    startSearch: TButton;
    displayImage: TCheckBox;
    displayInternalProperties: TCheckBox;
    detailPanel: TPanel;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    searchLocation: TComboBox;
    searchTitle: TComboBox;
    searchAuthor: TComboBox;
    searchISBN: TComboBox;
    searchSelectionList: TCheckListBox;
    Label1: TLabel;
    optionPanel: TPanel;
    bookListPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    procedure bookListSelect(sender: TObject; item: TTreeListItem);
    procedure displayImageChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Label12Click(Sender: TObject);
    procedure LabelSaveToClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure searchAuthorEnter(Sender: TObject);
    procedure searchAuthorExit(Sender: TObject);
    procedure searchTitleChange(Sender: TObject);
    procedure startSearchClick(Sender: TObject);
    procedure displayInternalPropertiesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure optionPanelClick(Sender: TObject);
    procedure searcherAccessDetailsComplete(sender: TObject; book: TBook);
    procedure searcherAccessException(Sender: TObject);
    procedure searcherAccessImageComplete(sender: TObject; book: TBook);
    procedure searcherAccessSearchComplete(Sender: TObject);
    procedure searchLocationSelect(Sender: TObject);
    procedure searchSelectionListClickCheck(Sender: TObject);
  private
    { private declarations }
    selectedLibrariesPerLocation: TStringList;
  public
    { public declarations }
    bookList: TBookListView;
    detaillist: TTreeListView;
    searcherAccess: TLibrarySearcherAccess;
    searchTemplates: TStringList; //TBookListTemplate;
    displayedBook: TBook;
    function displayDetails(book: TBook=nil): longint; //0: no details, 1: detail, no image, 2: everything
    procedure selectBookToReSearch(book: TBook);
    procedure loadDefaults;
    procedure saveDefaults;
  public
    saveToDefaultAccountID: string;
    procedure changeDefaultSaveToAccount(sender:tobject);
  end;

var
   bookSearchFrm: TbookSearchFrm;
const SB_PANEL_FOUND_COUNT=1;
      SB_PANEL_SEARCH_STATUS=0;
implementation

uses applicationconfig,libraryParser,simplexmlparser,bbdebugtools,bookWatchMain,bbutils,LCLType;

{ TbookSearchFrm }
//TODO: fehler bei keinem ergebnis

procedure TbookSearchFrm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  selectedLibrariesPerLocation:= TStringList.Create;
  loadDefaults;

  {list:=TList.Create;
  for i:=searchSelectionList.Items.count-1 downto 0 do begin
    libraryManager.enumerateLibrariesWithValue('Location',searchSelectionList.Items[i],list);
    for j:=list.count-1 downto 0 do
      searchSelectionList.items.InsertObject(i+1,'   '+tlibrary(list[j]).prettyNameLong,tobject(list[j]));
  end;
  list.free;  }
  

  bookList:=TBookListView.create(self,false);
  bookList.Parent:=bookListPanel;
  booklist.deserializeColumnOrder(userConfig.ReadString('BookSearcher','ListColumnOrder',''));
  booklist.deserializeColumnVisibility(userConfig.ReadString('BookSearcher','ListColumnVisibility','--+++----'));
  booklist.deserializeColumnWidths(userConfig.ReadString('BookSearcher','ListColumnWidths','10,10,200,200,50,'));
  booklist.OnSelect:=@bookListSelect;


  searcherAccess:=TLibrarySearcherAccess.Create;
  searcherAccess.OnSearchComplete:=@searcherAccessSearchComplete;
  searcherAccess.OnDetailsComplete:=@searcherAccessDetailsComplete;
  searcherAccess.OnImageComplete:=@searcherAccessImageComplete;
  searcherAccess.OnException:=@searcherAccessException;
  
  detaillist:=TTreeListView.Create(self);
  detaillist.Parent:=detailPanel;
  detaillist.align:=alClient;
  detaillist.columns.Clear;
  with detaillist.columns.Add do begin
    text:='Eigenschaftsname';
    width:=170;
  end;
  with detaillist.columns.Add do begin
    text:='Wert';
    width:=200;
  end;


  Image1.Width:=0;
end;

procedure TbookSearchFrm.startSearchClick(Sender: TObject);
  procedure addCbHistory(cb: TComboBox);
  var
    temp: TCaption;
    maxCount: LongInt;
  begin
    if (cb.text='') or (cb.Items.IndexOf(cb.Text)>=0) then exit;
    temp := cb.Text;
    cb.Items.Insert(0,temp);
    maxCount := userConfig.ReadInteger('BookSearcher', 'HistoryMaxLength', 15);
    while cb.Items.Count>maxCount do cb.Items.Delete(maxCount);
    cb.Text := temp; //lines above sometimes erase text
  end;
var i:longint;
begin
  displayedBook:=nil;
  i := searchTemplates.IndexOf(searchLocation.Text);
  if i = -1 then i := searchTemplates.IndexOf('digibib');
  if i = -1 then begin i := 0; if searchTemplates.Count = 0 then raise exception.Create('No search templates'); end;

  searcherAccess.newSearch(searchTemplates.Objects[i] as TBookListTemplate);
  searcherAccess.searcher.clear;
  for i:=0 to searchSelectionList.Items.count-1 do
    if searchSelectionList.Checked[i] and (searchSelectionList.Items.Objects[i]<>nil) then
      searcherAccess.searcher.addLibrary(searchSelectionList.Items.Objects[i] as tlibrary);
  searcherAccess.searcher.SearchOptions.author:=searchAuthor.Text;
  searcherAccess.searcher.SearchOptions.title:=searchTitle.Text;
  searcherAccess.searcher.SearchOptions.year:=searchYear.Text;
  searcherAccess.searcher.SearchOptions.isbn:=searchISBN.Text;
  setProperty('keywords', searchKeywords.Text, searcherAccess.searcher.SearchOptions.additional);
  if searchLocation.Text<>searcherAccess.searcher.Location then begin
    searcherAccess.searcher.setLocation(searchLocation.Text);
    searcherAccess.connectAsync;//digibib speziell, Suchmöglichkeiten ortabhängig
  end;

  
  searcherAccess.searchAsync;

  StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Suche Medien...';
  screen.Cursor:=crHourGlass;

  addCbHistory(searchAuthor);
  addCbHistory(searchTitle );
  addCbHistory(searchKeywords);
  addCbHistory(searchYear);
  addCbHistory(searchISBN);
end;

procedure TbookSearchFrm.displayInternalPropertiesChange(Sender: TObject);
begin
  displayDetails(nil);
end;

procedure TbookSearchFrm.bookListSelect(sender: TObject; item: TTreeListItem);
  procedure propAdd(n,v: string);
  begin
    if length(v)>1000 then v:='<'+IntToStr(length(v))+' Bytes ausgeblendet>';
    if (n<>'')and(n[length(n)]='!')and(v<>'') then
      detaillist.items.add((copy(n,1,length(n)-1))).RecordItems.Add((v))
     else if displayInternalProperties.Checked then
      detaillist.items.add((n)).RecordItems.Add((v))
  end;
var book: tbook;

begin
  if item=nil then exit;
  book:=tbook(item.data.obj);

  if displayDetails() < 1 then begin
    searcherAccess.detailsAsyncSave(book);
    Screen.Cursor:=crHourGlass;
    StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Suche Details für dieses Medium...';
  end;

end;

procedure TbookSearchFrm.displayImageChange(Sender: TObject);
begin
  displayDetails(nil);
end;

procedure TbookSearchFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  saveDefaults;
end;

procedure TbookSearchFrm.FormDeactivate(Sender: TObject);
begin

end;

procedure TbookSearchFrm.FormKeyUp(Sender: TObject; var Key: Word;
 Shift: TShiftState);
begin
 if key = VK_RETURN then startSearch.Click
 else if key = VK_ESCAPE then close
 else exit;
 key:=0;
end;

procedure TbookSearchFrm.Label12Click(Sender: TObject);
var temp:TBook;
    acc: TCustomAccountAccess;
    i: Integer;
begin
  if displayedBook = nil then exit;
  if accountIDs.Count = 0 then exit;
  searcherAccess.beginBookReading;
  temp := tbook.create;
  temp.assignNoReplace(displayedBook);
  temp.author:=displayedBook.author;
  temp.title:=displayedBook.title;
  temp.year:=displayedBook.year;
  temp.id:=displayedBook.id;
  searcherAccess.endBookReading;

  temp.issueDate:=-2;
  temp.limitDate:=-2;

  acc := TCustomAccountAccess(accountIDs.Objects[0]);
  for i:=1 to accountIDs.Count-1 do
    if accountIDs[i] = saveToDefaultAccountID then acc := TCustomAccountAccess(accountIDs.Objects[i]);
  if acc.isThreadRunning then begin ShowMessage('Während dem Aktualisieren können keine weiteren Medien gespeichert werden.'); exit; end;
  acc.books.old.add(temp);
  acc.save();
  mainForm.RefreshListView;
end;

procedure TbookSearchFrm.LabelSaveToClick(Sender: TObject);
begin
  saveToAccountMenu.PopUp;
end;

procedure TbookSearchFrm.Label2Click(Sender: TObject);
begin

end;

procedure TbookSearchFrm.searchAuthorEnter(Sender: TObject);
var hi: tcontrol;
begin
  if not (sender is tcontrol) then exit;
  hi:=FindComponent(tcontrol(sender).Name+'hint') as TControl;
  hi.Visible:=true;
end;

procedure TbookSearchFrm.searchAuthorExit(Sender: TObject);
var hi: tcontrol;
begin
  if not (sender is tcontrol) then exit;
  hi:=FindComponent(tcontrol(sender).Name+'hint') as TControl;
  hi.Visible:=false
end;

procedure TbookSearchFrm.searchTitleChange(Sender: TObject);
begin

end;

procedure TbookSearchFrm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  bookList.free;
  detaillist.free;
  searcherAccess.disconnectAsync;
  searcherAccess.free;
  for i:=0 to searchTemplates.Count - 1 do
    searchTemplates.Objects[i].Free;
  searchTemplates.free;
  selectedLibrariesPerLocation.free;
end;

procedure TbookSearchFrm.Image1Click(Sender: TObject);
begin

end;
procedure TbookSearchFrm.Label3Click(Sender: TObject);
var site: string;
begin
  if displayedBook=nil then begin
    ShowMessage('Kein Medium ausgewählt');
    exit;
  end;

  site:=(sender as tlabel).Caption;
  if pos('digibib',LowerCase(site))>0 then begin
    site:=getProperty('detail-url',displayedBook.additional);
  end else if pos('katalog',LowerCase(site))>0 then begin
    site:=getProperty('home-url',displayedBook.additional);
  end else if pos('amazon',LowerCase(site))>0 then begin
    site:=getProperty('amazon-url',displayedBook.additional);
  end else if pos('google',LowerCase(site))>0 then begin
    site:='http://www.google.de/search?q=%22'+displayedBook.title+'%22 %22'+displayedBook.author+'%22&ie=utf-8'
  end else begin
    ShowMessage('Gewünschte Seite nicht erkannt (wahrscheinlich Programmierfehler)');
    exit;
  end;
  
  if site='' then begin
    ShowMessage('Leider kann ich das ausgewählte Medium nicht in dieser Seite öffnen, da ich den nötigen Link nicht kenne');
    exit;
  end;
  if logging then log('Open page: '+site);
  openInternetPage(site);
end;

procedure TbookSearchFrm.optionPanelClick(Sender: TObject);
begin

end;

procedure TbookSearchFrm.searcherAccessDetailsComplete(sender: TObject;
  book: TBook);
begin
  if (displayDetails() < 2) and (displayImage.Checked) then begin
    searcherAccess.imageAsyncSave(displayedBook);
    StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Suche Titelbild...';
  end else begin
    StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='';
    screen.Cursor:=crDefault;
  end;
end;

procedure TbookSearchFrm.searcherAccessException(Sender: TObject);
begin
  mainForm.delayedCall.Enabled:=true;
end;

procedure TbookSearchFrm.searcherAccessImageComplete(sender: TObject;
  book: TBook);
begin
  displayDetails();
  StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='';
  screen.Cursor:=crDefault;
end;

procedure TbookSearchFrm.searcherAccessSearchComplete(Sender: TObject);
begin
  bookList.BeginUpdate;
  bookList.items.Clear;

  searcherAccess.beginResultReading;
  bookList.addBookList(searcherAccess.searcher.SearchResult);
  StatusBar1.Panels[SB_PANEL_FOUND_COUNT].text:=IntToStr(searcherAccess.searcher.SearchResult.Count)+' Treffer';
  searcherAccess.endResultReading;

  bookList.endupdate;

  StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Suche abgeschlossen';
  screen.Cursor:=crDefault;
end;

procedure TbookSearchFrm.searchLocationSelect(Sender: TObject);
var list: TList;
    i:longint;
begin
  searchSelectionList.Clear;
  list:=TList.Create;
  libraryManager.enumerateLibrariesWithValue('Location',searchLocation.Text,list);
  for i:= 0 to list.count-1  do
    searchSelectionList.items.AddObject(tlibrary(list[i]).prettyNameLong,tobject(list[i]));
  list.free;
  for i:=1 to min(length(selectedLibrariesPerLocation.Values[searchLocation.Text]),
                  searchSelectionList.Items.count) do
    searchSelectionList.Checked[i-1]:=selectedLibrariesPerLocation.Values[searchLocation.Text][i]='+';
end;

procedure TbookSearchFrm.searchSelectionListClickCheck(Sender: TObject);
var s:string;
    i:longint;
begin
  s:='';
  for i:=0 to searchSelectionList.Items.count-1 do
    if searchSelectionList.Checked[i] then s+='+'
    else s+='-';
  selectedLibrariesPerLocation.Values[searchLocation.Text]:=s;
end;

function TbookSearchFrm.displayDetails(book: TBook): longint;
var intern, empty, normal: TTreeListItems; //item lists
  procedure propAdd(n,v: string);
  begin
    if length(v)>1000 then v:='<'+IntToStr(length(v))+' Bytes ausgeblendet>';
    if not displayInternalProperties.Checked then begin
      if (n<>'')and(n[length(n)]='!')and(v<>'') then
        detaillist.items.add((copy(n,1,length(n)-1))).RecordItems.Add((v))
    end else begin
      if (n<>'')and(n[length(n)]='!')and(v<>'') then begin
        if normal=nil then normal:=detaillist.Items.Add('normale Eigenschaften').SubItems;
        normal.add((copy(n,1,length(n)-1))).RecordItems.Add((v))
      end else if (n<>'') and (n[length(n)]<>'!') then begin
        if intern=nil then intern:=detaillist.Items.Add('interne Eigenschaften').SubItems;
        intern.add((n)).RecordItems.Add((v));
      end else begin
        if empty=nil then empty:=detaillist.Items.Add('leere Eigenschaften').SubItems;
        empty.add((n)).RecordItems.Add((v))
      end;


    end;
  end;
var i:longint;
    tempStream: TStringStream;
begin
  if book=nil then
    if bookList.Selected=nil then book:=displayedBook
    else book:=tbook(bookList.Selected.data.obj);
  if book=nil then exit;
  intern:=nil;
  empty:=nil;
  normal:=nil;
  displayedBook:=book;
  Result:=0;
  searcherAccess.beginBookReading;
  try
    detaillist.BeginUpdate;
    detaillist.items.clear;
    propAdd('Id!',book.id);
    propAdd('Kategorie!',book.category);
    propAdd('Autor!',book.author);
    propAdd('Titel!',book.title);
    propAdd('Jahr!',book.year);
    propAdd('ISBN!',book.isbn);
    if book.owner<>nil then begin
      propAdd('Ausleihdatum!', DateToPrettyStr(book.issueDate));
      propAdd('Fristdatum!', DateToPrettyStr(book.limitDate));
      propAdd('Ausleihstatus!', BookStatusToStr(book,true));
      propAdd('Erstes Vorkommen',DateToPrettyStr(book.firstExistsDate));
      propAdd('Letztes Vorkommen',DateToPrettyStr(book.lastExistsDate));
    end;
    for i:=0 to high(book.additional) do
      propAdd(book.additional[i].name,book.additional[i].value);
    detaillist.EndUpdate;

    Image1.Picture.Clear;
    Image1.AutoSize:=false;
    Image1.Width:=1;
    if displayImage.Checked and (length(getProperty('image',book.additional))>50) then begin
      try
        tempStream:=TStringStream.Create(getProperty('image',book.additional));
        try
          image1.Picture.LoadFromStreamWithFileExt(tempStream, ExtractFileExt(getProperty('image-url',book.additional)));
          Image1.width:=Image1.Picture.Width;
        finally
          tempStream.free;
        end;
      except
        //sometimes the image is just garbage
      end;
    end;
    
    if getProperty('details-searched',book.additional) <> 'true' then Result:=0
    else if getProperty('image-searched',book.additional) <> 'true' then result:=1
    else result:=2;
  finally
    searcherAccess.endBookReading;
  end;
end;

procedure TbookSearchFrm.selectBookToReSearch(book: TBook);
var i,rp:longint;
    s: string;
begin
  displayDetails(book);
  searchAuthor.Text:=book.author;
  s:=book.title;
  for i:=length(s) downto 1 do
    if s[i] in [' ',',','.'] then s[i]:=' '
    else break;
  s:=trim(s);
  while striendswith(s,'Aufl') or striendswith(s,'Ausg') or striendswith(s,'Nachdr')
        or striendswith(s,'print') or striendswith(s,' ed') or striendswith(s,' pr') do begin
    rp:=0;
    for i:=length(s) downto 2 do
      if (s[i] = '.') and (s[i-1] in ['0'..'9']) then begin
        rp:=i-1;
        break;
      end;
    delete(s,rp,length(s)-rp+1);
    for i:=length(s) downto 1 do
      if s[i] in [' ',',','.'] then s[i]:=' '
      else break;
    s:=trim(s);
    if rp=0 then break;
  end;


  
  searchTitle.Text:=trim(s)+'*';
  if book.owner is TCustomAccountAccess then begin
    searchLocation.Text:=TCustomAccountAccess(book.owner).getLibrary().variables.Values['Location'];
    searchLocationSelect(self);
    for i:=0 to searchSelectionList.items.Count-1 do
      searchSelectionList.Checked[i]:=searchSelectionList.items.Objects[i]=TCustomAccountAccess(book.owner).getLibrary();
  end;
end;

procedure TbookSearchFrm.loadDefaults;
  procedure loadComboBoxItems(cb: TComboBox);
  begin
    cb.Items.Text:=StringReplace(
                      StringReplace(
                         userConfig.ReadString('BookSearcher','History'+cb.Name,''),
                         '\n',
                         LineEnding,
                         [rfReplaceAll]),
                      '\\','\',[rfReplaceAll]);
  end;
var sl: TStringList;
   i:longint;
begin
  sl:=TStringList.Create;
  libraryManager.enumerateVariableValues('Location',sl);
  for i := sl.count-1 downto 0 do begin
    sl[i] := trim(sl[i]);
    if sl[i] ='' then sl.delete(i);
  end;

  searchTemplates := TStringList.Create;
  searchTemplates.LoadFromFile(dataPath+StringReplace('libraries\search\search.list','\',DirectorySeparator,[rfReplaceAll]));
  for i := searchTemplates.count-1 downto 0 do begin
    searchTemplates[i] := trim(searchTemplates[i]);
    if searchTemplates[i] = '' then searchTemplates.Delete(i);
  end;
  for i :=0 to searchTemplates.count-1 do begin
    searchTemplates.Objects[i] := TBookListTemplate.create(dataPath+StringReplace('libraries\search\templates\'+trim(searchTemplates[i])+'\','\',DirectorySeparator,[rfReplaceAll]),trim(searchTemplates[i]));
    TBookListTemplate(searchTemplates.Objects[i]).loadTemplates;
    if searchTemplates[i] <> 'digibib' then sl.Add( searchTemplates[i] );
  end;

  for i:=0 to sl.count-1 do
    selectedLibrariesPerLocation.Values[sl[i]]:=userConfig.ReadString('BookSearcher','selection-'+sl[i],'+--');
  searchLocation.items.Assign(sl);
  if sl.IndexOf(userConfig.ReadString('BookSearcher', 'default-location', '')) > 0 then searchLocation.Text:=userConfig.ReadString('BookSearcher', 'default-location', '')
  else searchLocation.Text:=searchLocation.items[0];
  searchLocationSelect(self);
  sl.free;
  loadComboBoxItems(searchAuthor);
  loadComboBoxItems(searchTitle);
  loadComboBoxItems(searchKeywords);
  loadComboBoxItems(searchYear);
  loadComboBoxItems(searchISBN);
  saveToDefaultAccountID := userConfig.ReadString('BookSearcher','default-save-to', '');
  if accountIDs.IndexOf(saveToDefaultAccountID) >= 0 then
    LabelSaveTo.Caption := 'in \/ '+ TCustomAccountAccess(accountIDs.Objects[accountIDs.IndexOf(saveToDefaultAccountID)]).prettyName;
end;

procedure TbookSearchFrm.saveDefaults;
  procedure saveComboBoxItems(cb: TComboBox);
  begin
    userConfig.WriteString('BookSearcher','History'+cb.Name,
       StringReplace(
         StringReplace(cb.Items.Text,'\','\\',[rfReplaceAll]),
         LineEnding,'\n',[rfReplaceAll]));
  end;
var i:longint;
begin
  for i:=0 to selectedLibrariesPerLocation.Count-1 do
    userConfig.WriteString('BookSearcher','selection-'+selectedLibrariesPerLocation.Names[i],selectedLibrariesPerLocation.ValueFromIndex[i]);
  saveComboBoxItems(searchAuthor);
  saveComboBoxItems(searchTitle );
  saveComboBoxItems(searchKeywords);
  saveComboBoxItems(searchYear);
  saveComboBoxItems(searchISBN);
  userConfig.WriteString('BookSearcher','default-save-to', saveToDefaultAccountID);
  userConfig.WriteString('BookSearcher', 'default-location', searchLocation.Text);
end;

procedure TbookSearchFrm.changeDefaultSaveToAccount(sender: tobject);

begin
  if not (sender is TMenuItem) then exit;
  if tmenuitem(sender).Tag <= 1 then tmenuitem(sender).Tag := 1;
  if tmenuitem(sender).Tag > accountIDs.count then tmenuitem(sender).Tag := accountIDs.count;
  saveToDefaultAccountID := accountIDs[tmenuitem(sender).Tag-1];

  LabelSaveTo.Caption := 'in \/ '+ TMenuItem(sender).Caption;

  tmenuitem(sender).Checked:=true;
end;

initialization
  {$I bookSearchForm.lrs}
end.

