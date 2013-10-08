unit bookSearchForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  CheckLst, StdCtrls,bookListView, ComCtrls, Menus,librarySearcher,booklistreader,TreeListView,math,
  librarySearcherAccess,multipagetemplate;

type

  { TbookSearchFrm }

  TbookSearchFrm = class(TForm)
    startAutoSearchButton: TButton;
    searchAuthorHint: TLabel;
    searchTitleHint: TLabel;
    searchBranch: TComboBox;
    homeBranch: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2xx: TLabel;
    searchBranchLabel: TLabel;
    homeBranchLabel: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelOrder: TLabel;
    LabelOrderFor: TLabel;
    LabelSaveTo: TLabel;
    saveToAccountMenu: TPopupMenu;
    orderForAccountMenu: TPopupMenu;
    ScrollBox1: TScrollBox;
    searchAuthor: TComboBox;
    searchISBN: TComboBox;
    searchKeywords: TComboBox;
    searchTitle: TComboBox;
    searchYear: TComboBox;
    startSearch: TButton;
    displayImage: TCheckBox;
    displayInternalProperties: TCheckBox;
    detailPanel: TPanel;
    Image1: TImage;
    Label2: TLabel;
    linkLabelDigibib: TLabel;
    linkLabelBib: TLabel;
    linkLabelAmazon: TLabel;
    linkLabelGoogle: TLabel;
    Panel1: TPanel;
    searchLocation: TComboBox;
    searchSelectionList: TCheckListBox;
    Label1: TLabel;
    optionPanel: TPanel;
    bookListPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    autoSearchContinueTimer: TTimer;
    procedure autoSearchContinueTimerTimer(Sender: TObject);
    procedure bookListSelect(sender: TObject; item: TTreeListItem);
    procedure bookListVScrollBarChange(Sender: TObject);
    procedure startAutoSearchButtonClick(Sender: TObject);
    procedure detaillistClickAtRecordItem(sender: TObject; recorditem: TTreeListRecordItem);
    procedure detaillistCustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp; recordItem: TTreeListRecordItem;
      var defaultDraw: Boolean);
    procedure detaillistMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure searcherAccessConnected(Sender: TObject);
    procedure displayImageChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Label12Click(Sender: TObject);
    procedure LabelOrderClick(Sender: TObject);
    procedure LabelOrderForClick(Sender: TObject);
    procedure LabelSaveToClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure searchAuthorEnter(Sender: TObject);
    procedure searchAuthorExit(Sender: TObject);
    procedure searcherAccessOrderComplete(sender: TObject; book: TBook);
    procedure searcherAccessOrderConfirm(sender: TObject; book: TBook);
    procedure searcherAccessPendingMessageCompleted(Sender: TObject);
    procedure searcherAccessTakePendingMessage(sender: TObject; book: TBook; pendingMessage: TPendingMessage);
    procedure searchTitleChange(Sender: TObject);
    procedure startSearchClick(Sender: TObject);
    procedure displayInternalPropertiesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure linkLabelDigibibClick(Sender: TObject);
    procedure optionPanelClick(Sender: TObject);
    procedure searcherAccessDetailsComplete(sender: TObject; book: TBook);
    procedure searcherAccessException(Sender: TObject);
    procedure searcherAccessImageComplete(sender: TObject; book: TBook);
    procedure searcherAccessSearchComplete(sender: TObject; firstPage, nextPageAvailable: boolean);
    procedure searchLocationSelect(Sender: TObject);
    procedure searchSelectionListClickCheck(Sender: TObject);
  private
    { private declarations }
    locations: TSearchableLocations;
    selectedLibrariesPerLocation: TStringList;
    autoSearchPhase: (aspConnecting, aspConnected, aspSearching, aspSearched, aspSearchingDetails, aspSearchedDetails);
    autoSearchDetailCount: integer;
    function makeSearcherAccess: TLibrarySearcherAccess;
  public
    { public declarations }
    bookList: TBookListView;
    detaillist: TTreeListView;
    searcherAccess: TLibrarySearcherAccess;
    newSearcherAccess: TLibrarySearcherAccess;
    nextPageAvailable: boolean;
    displayedBook: TBook;
    researchedBook: TBook;
    function displayDetails(book: TBook=nil): longint; //0: no details, 1: detail, no image, 2: everything
    function cloneDisplayedBook: TBook;
    procedure selectBookToReSearch(book: TBook);
    procedure loadDefaults;
    procedure saveDefaults;
  public
    saveToDefaultAccountID, orderForDefaultAccountID: string;
    procedure changeDefaultSaveToAccount(sender:tobject);
    procedure changeDefaultOrderForAccount(sender:tobject);
  end;

var
   bookSearchFrm: TbookSearchFrm;
const SB_PANEL_FOUND_COUNT=1;
      SB_PANEL_SEARCH_STATUS=0;
implementation

uses applicationconfig,libraryParser,simplexmlparser,bbdebugtools,bookWatchMain,bbutils,LCLType,libraryAccess;

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
  bookList.OnVScrollBarChange:=@bookListVScrollBarChange;

  searcherAccess := nil;

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
  detaillist.OnCustomRecordItemDraw:=@detaillistCustomRecordItemDraw;
  detaillist.OnClickAtRecordItem:=@detaillistClickAtRecordItem;
  detaillist.OnMouseMove:=@detaillistMouseMove;


  Image1.Width:=0;

  if debugMode then begin
    startAutoSearchButton.Visible := true;
  end;
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
var i, j:longint;
  first: Boolean;
begin
  displayedBook:=nil;
  nextPageAvailable:=false;

  if newSearcherAccess <> nil then begin
    if (searcherAccess <> nil) then begin
      searcherAccess.free;;
    end;
    searcherAccess := newSearcherAccess;
    newSearcherAccess := nil;
  end;

  i := locations.locations.IndexOf(searchLocation.Text);
  if i = -1 then begin i := 0; if locations.locations.Count = 0 then raise exception.Create('No search templates'); end;

  screen.Cursor:=crHourGlass;

  StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Warte...';

  searcherAccess.prepareNewSearchWithoutDisconnect;

  if homeBranch.ItemIndex >= 0 then searcherAccess.searcher.HomeBranch:=homeBranch.ItemIndex;
  if searchBranch.ItemIndex >= 0 then searcherAccess.searcher.SearchBranch:=searchBranch.ItemIndex;

  searcherAccess.searcher.SearchOptions.author:=searchAuthor.Text;
  searcherAccess.searcher.SearchOptions.title:=searchTitle.Text;
  searcherAccess.searcher.SearchOptions.year:=searchYear.Text;
  searcherAccess.searcher.SearchOptions.isbn:=searchISBN.Text;
  setProperty('keywords', searchKeywords.Text, searcherAccess.searcher.SearchOptions.additional);


  searcherAccess.searchAsync;

  StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Suche Medien...';

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
  if searcherAccess = nil then exit;
  book:=tbook(item.data.obj);

  if displayDetails() < 1 then begin
    searcherAccess.detailsAsyncSave(book);
    Screen.Cursor:=crHourGlass;
    StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Suche Details für dieses Medium...';
  end;

  linkLabelDigibib.Enabled := getProperty('digibib-url', book.additional) <> '';
  linkLabelBib.Enabled := getProperty('home-url', book.additional) <> '';
  linkLabelAmazon.Enabled := getProperty('amazon-url', book.additional) <> '';
end;

procedure TbookSearchFrm.autoSearchContinueTimerTimer(Sender: TObject);
var
  i: Integer;
begin
//  if searcherAccess.operationActive then exit;
  case autoSearchPhase of
    aspConnected: begin
      autoSearchPhase:=aspSearching;
      startSearch.Click;
      exit;
    end;
    aspSearched: begin
      autoSearchDetailCount := min(3, bookList.VisibleRowCount);
    end;
    aspSearchedDetails: begin
      autoSearchDetailCount -= 1;
    end
    else exit;
  end;
  if autoSearchDetailCount <= 0 then begin
    if searchSelectionList.Checked[searchSelectionList.Count-1] then begin
      if searchLocation.ItemIndex = searchLocation.Items.Count - 1 then begin
        autoSearchContinueTimer.Enabled := false;
        exit;
      end;
      searchLocation.ItemIndex := searchLocation.ItemIndex + 1;
      searchLocationSelect(searchLocation);
      for i := 0 to searchSelectionList.Count - 1 do
        searchSelectionList.Checked[i] := false;
      searchSelectionList.Checked[0] := true;

    end else begin
      for i := 0 to searchSelectionList.Count - 2 do
        if searchSelectionList.Checked[i] then begin
          searchSelectionList.Checked[i]  := false;
          searchSelectionList.Checked[i + 1] := true;
          break;
        end;
    end;
    autoSearchPhase:=aspConnecting;
    searchSelectionListClickCheck(searchSelectionList);
    exit;
  end;
                                     ;
//  if bookList.Selected = nil then bookList.Selected := ;
autoSearchPhase:=aspSearchingDetails;
  bookList.Selected := bookList.Items[bookList.Items.IndexOf(bookList.Selected)+1];

end;

procedure TbookSearchFrm.bookListVScrollBarChange(Sender: TObject);
begin
  if nextPageAvailable
     and (bookList.VisibleRowCount > bookList.Items.Count - bookList.TopItemVisualIndex)
     and (searcherAccess<>nil)
     then begin
    nextPageAvailable := false;
    searcherAccess.searchNextAsync;
    StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Lade nächste Seite...';
  end;
end;

procedure TbookSearchFrm.startAutoSearchButtonClick(Sender: TObject);
var
  i: Integer;
begin
  autoSearchContinueTimer.Enabled:=true;
 // searchLocation.ItemIndex := 1;
 // searchLocationSelect(searchLocation);
  for i := 0 to searchSelectionList.Count - 1 do
    searchSelectionList.Checked[i] := false;
  searchSelectionList.Checked[0] := true;
  searchSelectionListClickCheck(searchSelectionList);
end;

procedure TbookSearchFrm.detaillistClickAtRecordItem(sender: TObject; recorditem: TTreeListRecordItem);
var
  i: SizeInt;
  temp: String;
begin
  if strContains(recordItem.Text, 'http://') or strContains(recordItem.Text, 'https://') then begin
    i := pos('http://', recordItem.Text);
    if i < 0 then i := pos('https://', recordItem.Text);
    temp := strCopyFrom(recorditem.Text, i);
    if strContains(temp, ' ') then delete(temp, strIndexOf(temp, ' '), length(temp));
    openInternetPage(temp);
  end;
end;

procedure TbookSearchFrm.detaillistCustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp;
  recordItem: TTreeListRecordItem; var defaultDraw: Boolean);
begin
  if strContains(recordItem.Text, 'http://') or strContains(recordItem.Text, 'https://') then ttreelistview(sender).Canvas.Font.Color:=clBlue
  else ttreelistview(sender).Canvas.Font.Color:=clBlack;
end;

procedure TbookSearchFrm.detaillistMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  tlv: TTreeListView;
  recordItem: TTreeListRecordItem;
begin
  tlv := TTreeListView(sender);
  recordItem := tlv.GetRecordItemAtPos(x,y);
  if (recorditem <> nil) and (strContains(recordItem.Text, 'http://') or strContains(recordItem.Text, 'https://')) then
     tlv.Cursor:=crHandPoint
    else
     tlv.Cursor:=crDefault;
end;

procedure TbookSearchFrm.searcherAccessConnected(Sender: TObject);
  procedure update(cb: TComboBox; sa: TStringArray; l: TLabel);
  var
    i: Integer;
  begin
    l.Visible:=length(sa) > 0;
    cb.Visible:=length(sa) > 0;
    cb.Clear;
    if length(sa) = 0 then exit;
    for i := 0 to high(sa) do
      cb.Items.Add(sa[i]);
    if cb.ItemIndex < 0 then cb.ItemIndex:=0;
  end;

begin
  if sender <> newSearcherAccess then exit;
  update(homeBranch, newSearcherAccess.searcher.HomeBranches, homeBranchLabel);
  update(searchBranch, newSearcherAccess.searcher.SearchBranches, searchBranchLabel);
  autoSearchPhase:=aspConnected;
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
var temp, old:TBook;
    acc: TCustomAccountAccess;
    i: Integer;
begin
  if displayedBook = nil then exit;
  if accounts.Count = 0 then exit;

  acc := accounts[0];
  for i:=1 to accounts.Count-1 do
    if accounts.Strings[i] = saveToDefaultAccountID then acc := accounts[i];
  if acc.isThreadRunning then begin ShowMessage('Während dem Aktualisieren können keine weiteren Medien gespeichert werden.'); exit; end;

  temp := cloneDisplayedBook;

  temp.issueDate:=-2;
  temp.dueDate:=-2;


  if (mainForm.BookList.SelectedBook <> nil) and (mainForm.BookList.Selected.RecordItemsText[BL_BOOK_COLUMNS_ACCOUNT] = acc.prettyName) then begin
    old := mainForm.BookList.SelectedBook;
    if (old.author = temp.author) or (old.title = temp.title) or
       striBeginsWith(temp.author,old.author) or striBeginsWith(temp.title, old.title) then
     if confirm('Das Medium existiert bereits als "'+old.toSimpleString()+'", soll es mit "'+temp.toSimpleString()+'" überschrieben werden?') then begin
       old.author:=temp.author; //don't copy id
       old.title:=temp.title;
       old.year:=temp.year;
       old.isbn:=temp.isbn;
       old.assignNoReplace(temp);
       acc.save();
       mainForm.RefreshListView;
       exit;
     end;
    if researchedBook <> nil then
     if confirm('Soll das markierte Medium "'+old.toSimpleString()+'" mit "'+temp.toSimpleString()+'" überschrieben werden?') then begin
       old.author:=temp.author; //don't copy id
       old.title:=temp.title;
       old.year:=temp.year;
       old.isbn:=temp.isbn;
       old.assignNoReplace(temp);
       acc.save();
       mainForm.RefreshListView;
       researchedBook := nil;
       exit;
     end;
  end;

  acc.books.old.add(temp);
  acc.save();
  mainForm.RefreshListView;
  researchedBook := nil;
end;

procedure TbookSearchFrm.LabelOrderClick(Sender: TObject);
var
  temp: TBook;
  acc: TCustomAccountAccess;
  tempList: TBookList;
  i: Integer;
  v: String;
  s: String;
begin
  if (displayedBook = nil) or (searcherAccess = nil) then exit;
  if accounts.Count = 0 then exit;

  acc := accounts[0];
  for i:=1 to accounts.Count-1 do
   if accounts.Strings[i] = orderForDefaultAccountID then acc := accounts[i];
  if acc.isThreadRunning then begin ShowMessage('Während dem Aktualisieren/Verlängern/Vormerken können keine weiteren Medien vorgemerkt werden.'); exit; end;

  searcherAccess.beginBookReading;
  try
    if StrToIntDef(displayedBook.getPropertyAdditional('orderable'), 1) > 1 then begin
      s := 'Es gibt mehrere vormerkbare/Bestellbare Exemplare. Welches wollen Sie? (Nummer eingeben)'+LineEnding;
      for i :=  0 to StrToIntDef(displayedBook.getPropertyAdditional('orderable'), 1) - 1 do
       s += inttostr(i+1)+': '+ displayedBook.getPropertyAdditional('orderTitle'+inttostr(i))+LineEnding;
      v := '1';
      if not InputQuery('VideLibri', s, v) then exit;
      displayedBook.setProperty('choosenOrder', inttostr(strtointdef(v,1)-1));
    end else displayedBook.setProperty('choosenOrder', '0');
  finally
    searcherAccess.endBookReading;
  end;


  screen.Cursor:=crHourGlass;
  searcherAccess.orderAsync(acc, displayedBook);

  {temp := cloneDisplayedBook;

  temp.owner := acc;
  tempList := TBookList.create(acc);
  tempList.add(temp);

  orderBooks(tempList);}
end;

procedure TbookSearchFrm.LabelOrderForClick(Sender: TObject);
begin
  orderForAccountMenu.PopUp();
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

procedure TbookSearchFrm.searcherAccessOrderComplete(sender: TObject; book: TBook);
var
  acc: TCustomAccountAccess;
  temp: TBook;
begin
  if sender <> searcherAccess then exit;
  //see androidutils/bookSearchForm
  searcherAccess.beginBookReading;
  EnterCriticalSection(updateThreadConfig.libraryAccessSection);
  acc := TCustomAccountAccess(book.owner);
  temp := book.clone; temp.status:=bsOrdered;
  acc.books.current.add(temp);
  temp := book.clone; temp.status:=bsOrdered;
  acc.books.currentUpdate.add(temp); //cannot know which one is the correct one? one will be discarded?
  acc.save();
  LeaveCriticalSection(updateThreadConfig.libraryAccessSection);
  searcherAccess.endBookReading;


  if mainForm <> nil then mainForm.RefreshListView;
  ShowMessage(format('Das Buch "%s" wurde ohne Fehler vorgemerkt.', [book.toSimpleString()])  );
  screen.Cursor:=crDefault;
end;

procedure TbookSearchFrm.searcherAccessOrderConfirm(sender: TObject; book: TBook);
var
  question: String;
  orderConfirmationOptionTitles: String;
  temp: TStringArray;
  i: Integer;
  v: string;
begin
  if sender <> searcherAccess then exit;

  searcherAccess.beginBookReading;
  question := book.getPropertyAdditional('orderConfirmation');
  orderConfirmationOptionTitles := book.getPropertyAdditional('orderConfirmationOptionTitles');
  searcherAccess.endBookReading;

  question := StringReplace(question, '\n', LineEnding, [rfReplaceAll]);
  if orderConfirmationOptionTitles <> '' then begin
    question += ' (Nummer eingeben)';
    temp := strSplit(orderConfirmationOptionTitles, '\|');
    for i := 0 to high(temp) do
      question += LineEnding + IntToStr(i+1) +':'  + temp[i];
    v := '1';
    screen.Cursor:=crDefault;
    if not InputQuery('VideLibri', question, v) then exit;
    screen.Cursor:=crHourGlass;

    searcherAccess.beginBookReading;
    book.setProperty('choosenConfirmation', inttostr(strtointdef(v,1)));
    searcherAccess.endBookReading;

    searcherAccess.orderConfirmedAsync(book);
  end else
    if (question = '') or confirm(question) then searcherAccess.orderConfirmedAsync(book);
end;

procedure TbookSearchFrm.searcherAccessPendingMessageCompleted(Sender: TObject);
begin
  screen.Cursor:=crDefault;
end;

procedure TbookSearchFrm.searcherAccessTakePendingMessage(sender: TObject; book: TBook; pendingMessage: TPendingMessage);
var
  question: String;
  v: AnsiString;
  i: Integer;
begin
  screen.Cursor:=crDefault;
  case pendingMessage.kind of
    pmkChoose: begin
      question := pendingMessage.caption + ' (Nummer eingeben)';
      for i := 0 to high(pendingMessage.options) do
        question += LineEnding + IntToStr(i+1) +':'  + pendingMessage.options[i];
      v := '1';
      if not InputQuery('VideLibri', question, v) then v := '0';
      screen.Cursor:=crHourGlass;

      searcherAccess.completePendingMessage(book, pendingMessage, StrToIntDef(v, 0) - 1);
    end;
    pmkConfirm: begin
      searcherAccess.completePendingMessage(book, pendingMessage, ifthen(confirm(pendingMessage.caption), 1, 0));
    end;
  end;
end;

procedure TbookSearchFrm.searcherAccessSearchComplete(sender: TObject; firstPage, nextPageAvailable: boolean);
begin
  if sender <> searcherAccess then exit;

  bookList.BeginUpdate;
  if firstPage then bookList.items.Clear;
  self.nextPageAvailable := nextPageAvailable;;

  searcherAccess.beginResultReading;
  bookList.addBookList(searcherAccess.searcher.SearchResult);
  if (bookList.Items.Count = searcherAccess.searcher.SearchResultCount) or (searcherAccess.searcher.SearchResultCount = 0) then
    StatusBar1.Panels[SB_PANEL_FOUND_COUNT].text:=IntToStr(bookList.Items.Count)+' Treffer'
  else
    StatusBar1.Panels[SB_PANEL_FOUND_COUNT].text:=Format('%D / %D Treffer', [bookList.Items.Count, searcherAccess.searcher.SearchResultCount]);
  searcherAccess.endResultReading;

  bookList.endupdate;

  if (bookList.TopItemVisualIndex + bookList.VisibleRowCount > bookList.Items.Count) and (nextPageAvailable) then begin
    searcherAccess.searchNextAsync;
    StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Lade nächste Seite...';
  end else begin
    if nextPageAvailable then
      StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Suche abgeschlossen (mehr Ergebnisse verfügbar)'
    else
      StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Suche abgeschlossen';
    autoSearchPhase:=aspSearched;
  end;

  screen.Cursor:=crDefault;


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
  searcherAccess.free;
  newSearcherAccess.free;
  for i:=0 to locations.searchTemplates.Count - 1 do
    locations.searchTemplates.Objects[i].Free;
  locations.searchTemplates.free;
  for i:=0 to locations.locations.Count - 1 do
    locations.locations.Objects[i].Free;
  locations.locations.Free;
  selectedLibrariesPerLocation.free;
end;

procedure TbookSearchFrm.Image1Click(Sender: TObject);
begin

end;
procedure TbookSearchFrm.linkLabelDigibibClick(Sender: TObject);
var site: string;
begin
  if displayedBook=nil then begin
    ShowMessage('Kein Medium ausgewählt');
    exit;
  end;

  site:=(sender as tlabel).Caption;
  if pos('digibib',LowerCase(site))>0 then begin
    site:=getProperty('digibib-url',displayedBook.additional);
  end else if (pos('katalog',LowerCase(site))>0) or (pos('bücherei',LowerCase(site))>0) then begin
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
  if sender <> searcherAccess then exit;
  if (displayDetails() < 2) and (displayImage.Checked) then begin
    searcherAccess.imageAsyncSave(displayedBook);
    StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='Suche Titelbild...';
  end else begin
    StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='';
    screen.Cursor:=crDefault;
    autoSearchPhase:=aspSearchedDetails;
  end;
end;

procedure TbookSearchFrm.searcherAccessException(Sender: TObject);
begin
  if (sender <> searcherAccess) and (sender <> newSearcherAccess) then exit;
  mainForm.delayedCall.Enabled:=true;
end;

procedure TbookSearchFrm.searcherAccessImageComplete(sender: TObject;
  book: TBook);
begin
  if sender <> searcherAccess then exit;
  displayDetails();
  StatusBar1.Panels[SB_PANEL_SEARCH_STATUS].Text:='';
  autoSearchPhase:=aspSearchedDetails;
  screen.Cursor:=crDefault;
end;


procedure TbookSearchFrm.searchLocationSelect(Sender: TObject);
var list: TList;
    i:longint;
    loki: Integer;
begin
  loki := locations.locations.IndexOf(searchLocation.Text);
  searchSelectionList.Items.Assign(TStringList(locations.locations.Objects[loki]));

  for i:=1 to min(length(selectedLibrariesPerLocation.Values[searchLocation.Text]),
                  searchSelectionList.Items.count) do
    searchSelectionList.Checked[i-1]:=selectedLibrariesPerLocation.Values[searchLocation.Text][i]='+';

  newSearcherAccess := makeSearcherAccess;
end;

procedure TbookSearchFrm.searchSelectionListClickCheck(Sender: TObject);
var s:string;
    i:longint;
    oldState: String;
    last: LongInt;
    disallowMultiselect: Boolean;
begin
  oldState := selectedLibrariesPerLocation.Values[searchLocation.Text];
  disallowMultiselect := pos('digibib', searchLocation.Text) = 0;
  s:='';
  last := 0;
  for i:=0 to searchSelectionList.Items.count-1 do begin
    if searchSelectionList.Checked[i] then s+='+'
    else s+='-';
    if disallowMultiselect and searchSelectionList.Checked[i] and (i + 1 <= length(oldState)) and (oldState[i+1] = '+') then begin
      s[length(s)] := '-';
      last := i;
      searchSelectionList.Checked[i] := false;
    end;
  end;
  if (pos('+', s) = 0) and (last < searchSelectionList.Items.count) then begin
    searchSelectionList.Checked[last] := true;
    s[last+1] := '+';
  end;

  selectedLibrariesPerLocation.Values[searchLocation.Text]:=s;


  newSearcherAccess := makeSearcherAccess;
end;

function TbookSearchFrm.makeSearcherAccess: TLibrarySearcherAccess;
var
  first: Boolean;
  j: Integer;
  i: Integer;
begin
  result:=TLibrarySearcherAccess.Create;
  result.OnSearchPageComplete:=@searcherAccessSearchComplete;
  result.OnDetailsComplete:=@searcherAccessDetailsComplete;
  result.OnOrderComplete:=@searcherAccessOrderComplete;
  result.OnOrderConfirm:=@searcherAccessOrderConfirm;
  result.OnTakePendingMessage:=@searcherAccessTakePendingMessage;
  result.OnPendingMessageCompleted:=@searcherAccessPendingMessageCompleted;
  result.OnImageComplete:=@searcherAccessImageComplete;
  result.OnException:=@searcherAccessException;
  result.OnConnected:=@searcherAccessConnected;
  newSearcherAccess := result;

  i := locations.locations.IndexOf(searchLocation.Text);
  if i = -1 then exit;

  first := true;
  for j := 0 to searchSelectionList.Items.count - 1 do
    if searchSelectionList.Checked[j] then begin
      if first then begin
        result.newSearch( TSearchTarget((TStringList(locations.locations.Objects[i])).Objects[j]).template );
        result.searcher.clear;
        first := false;
      end;
      result.searcher.addLibrary(TSearchTarget((TStringList(locations.locations.Objects[i])).Objects[j]).lib);
    end;

  if first then begin
    if TStringList(locations.locations.Objects[i]).count = 0 then exit;
    result.newSearch( TSearchTarget((TStringList(locations.locations.Objects[i])).Objects[0]).template );
    result.searcher.clear;
    result.searcher.addLibrary(TSearchTarget((TStringList(locations.locations.Objects[i])).Objects[0]).lib);
    if searchSelectionList.Count > 0 then
      searchSelectionList.Checked[0] := true;
  end;

  result.searcher.setLocation(searchLocation.Text);
  result.connectAsync;
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
    ext: String;
begin
  if book=nil then
    if bookList.Selected=nil then book:=displayedBook
    else book:=tbook(bookList.Selected.data.obj);
  if (book=nil) or (searcherAccess = nil) then exit;
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
    if getProperty('publisher', book.additional) <> '' then propAdd('Verlag!', getProperty('publisher', book.additional));
    if getProperty('location', book.additional) <> '' then propAdd('Ort!', getProperty('location', book.additional));
    if book.owner<>nil then begin
      propAdd('Ausleihdatum!', DateToPrettyStr(book.issueDate));
      propAdd('Fristdatum!', DateToPrettyStr(book.dueDate));
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
          ext := getProperty('image-content-type',book.additional);
          if striContains(ext, 'image/jpeg') or striContains(ext, 'image/jpg') then ext := '.jpg'
          else if striContains(ext, 'image/png') then ext := '.png'
          else if striContains(ext, 'image/gif') then ext := '.gif'
          else ext := ExtractFileExt(getProperty('image-url',book.additional));
          image1.Picture.LoadFromStreamWithFileExt(tempStream, ext);
          Image1.width:=min(image1.Picture.Width, Image1.Picture.Width * image1.Height div max(1,image1.Picture.Height));
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

    LabelOrder.Enabled:=(getProperty( 'orderable', book.additional) <> '') and (getProperty('orderable', book.additional) <> '0') and (getProperty('orderable', book.additional) <> 'false');
    LabelOrder.Caption := book.getPropertyAdditional('orderTitle', 'Vormerken/Bestellen');
    linkLabelBib.Enabled := getProperty('home-url', book.additional) <> '';
    linkLabelAmazon.Enabled := getProperty('amazon-url', book.additional) <> '';
  finally
    searcherAccess.endBookReading;
  end;
end;

function TbookSearchFrm.cloneDisplayedBook: TBook;
begin
  if searcherAccess = nil then exit;
  searcherAccess.beginBookReading;
  result := displayedBook.clone;
  searcherAccess.endBookReading;
end;

procedure TbookSearchFrm.selectBookToReSearch(book: TBook);
  function selectLibrary(location: string; lib: TLibrary): boolean;
  var
    state: String;
    i: Integer;
  begin
    if searchLocation.Items.IndexOf(location) < 0 then exit(false);
    searchLocation.Text:=location;
    searchLocationSelect(self);

    state := '';
    for i:=0 to searchSelectionList.items.Count-1 do begin
      searchSelectionList.Checked[i]:=TSearchTarget(searchSelectionList.items.Objects[i]).lib=lib;
      if searchSelectionList.Checked[i] then state += '+'
      else state += '-';
    end;
    selectedLibrariesPerLocation.Values[searchLocation.Text]:=state;

    result := pos('+', state) > 0;

    newSearcherAccess := makeSearcherAccess;
  end;

var i,rp:longint;
    s: string;
    accId: Integer;
    found: Boolean;
begin
  researchedBook := nil;
  if book = nil then
    exit;

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
    if not selectLibrary(TCustomAccountAccess(book.owner).getLibrary().prettyLocation, TCustomAccountAccess(book.owner).getLibrary()) then
      selectLibrary(TCustomAccountAccess(book.owner).getLibrary().prettyLocation+ ' (digibib)', TCustomAccountAccess(book.owner).getLibrary());

    accId := accounts.IndexOfObject(book.owner);
    if (accId >= 0) and (accId < saveToAccountMenu.Items.Count) then begin
      changeDefaultSaveToAccount(saveToAccountMenu.Items[accId]);
      researchedBook := book;
    end;
    if (accId >= 0) and (accId < orderForAccountMenu.Items.Count) then begin
      changeDefaultOrderForAccount(orderForAccountMenu.Items[accId]);
    end;
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
var i:longint;
   location: String;
   temp: TMultiPageTemplate;
begin
  locations := getSearchableLocations;

  searchLocation.Items.Clear;
  for i := 0 to locations.locations.count - 1 do
    searchLocation.Items.add(locations.locations[i]);


  for i:=0 to locations.locations.count-1 do
    selectedLibrariesPerLocation.Values[locations.locations[i]]:=userConfig.ReadString('BookSearcher','selection-'+locations.locations[i],'+--');

  if locations.locations.IndexOf(userConfig.ReadString('BookSearcher', 'default-location', '')) > 0 then
    searchLocation.Text:=userConfig.ReadString('BookSearcher', 'default-location', '')
  else
    searchLocation.Text:=searchLocation.items[0];
  searchLocationSelect(self);

  loadComboBoxItems(searchAuthor);
  loadComboBoxItems(searchTitle);
  loadComboBoxItems(searchKeywords);
  loadComboBoxItems(searchYear);
  loadComboBoxItems(searchISBN);
  saveToDefaultAccountID := userConfig.ReadString('BookSearcher','default-save-to', '');
  if accounts.IndexOf(saveToDefaultAccountID) >= 0 then
    LabelSaveTo.Caption := 'in \/ '+ accounts[accounts.IndexOf(saveToDefaultAccountID)].prettyName;
  orderForDefaultAccountID := userConfig.ReadString('BookSearcher','default-order-for', '');
  if accounts.IndexOf(orderForDefaultAccountID) >= 0 then
    LabelOrderFor.Caption := 'für \/ '+ accounts[accounts.IndexOf(orderForDefaultAccountID)].prettyName;
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
  userConfig.WriteString('BookSearcher','default-order-for', orderForDefaultAccountID);
  userConfig.WriteString('BookSearcher', 'default-location', searchLocation.Text);
end;

procedure TbookSearchFrm.changeDefaultSaveToAccount(sender: tobject);

begin
  if not (sender is TMenuItem) then exit;
  if tmenuitem(sender).Tag <= 1 then tmenuitem(sender).Tag := 1;
  if tmenuitem(sender).Tag > accounts.count then tmenuitem(sender).Tag := accounts.count;
  saveToDefaultAccountID := accounts.Strings[tmenuitem(sender).Tag-1];

  LabelSaveTo.Caption := 'in \/ '+ TMenuItem(sender).Caption;

  tmenuitem(sender).Checked:=true;
end;

procedure TbookSearchFrm.changeDefaultOrderForAccount(sender: tobject);

begin
  if not (sender is TMenuItem) then exit;
  if tmenuitem(sender).Tag <= 1 then tmenuitem(sender).Tag := 1;
  if tmenuitem(sender).Tag > accounts.count then tmenuitem(sender).Tag := accounts.count;

  orderForDefaultAccountID := accounts.Strings[tmenuitem(sender).Tag-1];
  LabelOrderFor.Caption := 'für \/ '+ TMenuItem(sender).Caption;

  tmenuitem(sender).Checked:=true;
end;

initialization
  {$I bookSearchForm.lrs}
end.

