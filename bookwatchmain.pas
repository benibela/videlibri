unit bookWatchMain;
{$mode objfpc}{$H+}


{$WARN 5024 off : Parameter "$1" not used}
interface
                                      
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, libraryParser, internetAccess, ComCtrls, Menus, lmessages, ExtCtrls,
  errorDialog, statistik_u, libraryAccess, sendBackError, Translations,
  progressDialog, bookListView, TreeListView, bookSearchForm, LCLType, lclproc,
  LCLIntf, process, applicationconfig,exportxml,applicationdesktopconfig,applicationformconfig;

const //automaticExtend=true;
      colorSelected=clHighlight;
      colorSelectedText=clHighlightText;
      colorStandardText=clBlack;
//const WM_ThreadException=wm_user+$321;

type
  { TmainForm }
  TmainForm = class(TVideLibriForm)
    accountListMenuItem: TMenuItem;
    cancelTheseBooks: TMenuItem;
    groupingItem: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem11: TMenuItem;
    menuDuplicateChecker: TMenuItem;
    menuXquery: TMenuItem;
    MenuItemDebugLog: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItemTester: TMenuItem;
    MenuItem10: TMenuItem;
    extendMenuList1: TMenuItem;
    extendTheseBooks: TMenuItem;
    helpMenu: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    extendMenuList2_: TMenuItem;
    extendMenuList2_1: TMenuItem;
    extendMenuList2_4: TMenuItem;
    extendMenuList2_3: TMenuItem;
    extendMenuList2_2: TMenuItem;
    extendAdjacentBooks: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    EnsureTrayIconTimer: TTimer;
    MenuItem31: TMenuItem;
    repeatedCheckTimer: TTimer;
    dailyCheckThread: TTimer;
    androidActivationTimer: TTimer;
    TrayIconClick: TTimer;
    trayIconPopupMenu: TPopupMenu;
    removeSelectedMI: TMenuItem;
    displayDetailsMI: TMenuItem;
    searchDetailsMI: TMenuItem;
    MenuItem3: TMenuItem;
    bookMenu: TMenuItem;
    bookPopupMenu: TPopupMenu;

    MenuItem8: TMenuItem;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    extraMenu: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    showAllAccounts: TMenuItem;
    showNoAccounts: TMenuItem;
    TrayIcon1: TTrayIcon;
    viewMenu: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    accountListMenu: TPopupMenu;
    delayedCall: TTimer;
    libraryList: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    btnRefresh: TToolButton;
    ToolButton3: TToolButton;
    ViewCurrent: TMenuItem;
    ViewAll: TMenuItem;
    ViewOld: TMenuItem;
    StatusBar1: TStatusBar;
    procedure accountAddedChanged(acc: TCustomAccountAccess);
    procedure accountsChanged(Sender: TObject);
    procedure androidActivationTimerTimer(Sender: TObject);
    procedure bookPopupMenuPopup(Sender: TObject);
    procedure BookListUserSortItemsEvent(sender: TObject;
      var sortColumn: longint; var invertSorting: boolean);
    procedure cancelTheseBooksClick(Sender: TObject);
    procedure dailyCheckThreadTimer(Sender: TObject);
    procedure delayedCallTimer(Sender: TObject);
    procedure EnsureTrayIconTimerTimer(Sender: TObject);
    procedure extendAdjacentBooksClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BookListSelectItem(Sender: TObject; Item: TTreeListItem);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure menuDuplicateCheckerClick(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItemDebugLogClick(Sender: TObject);
    procedure MenuItemRenewClick(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure MenuItemTesterClick(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure MenuItem31Click(Sender: TObject);
    procedure menuXqueryClick(Sender: TObject);
    procedure newAccountWizardClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure removeSelectedMIClick(Sender: TObject);
    procedure displayDetailsMIClick(Sender: TObject);
    procedure repeatedCheckTimerTimer(Sender: TObject);
    procedure searchTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TrayIcon1Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure TrayIconClickTimer(Sender: TObject);
    procedure TreeListView1Click(Sender: TObject);
    procedure updateThreadConfigPartialUpdated(Sender: TObject);
    procedure UserExtendMenuClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure searchCloseClick(Sender: TObject);
    procedure searchTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure searchUpClick(Sender: TObject);
    procedure showAccount(Sender: TObject);
    procedure refreshAccountClick(Sender: TObject);
    procedure LibraryHomepageClick(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ShowOptionsClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure ToolButton2ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ToolButton3Click(Sender: TObject);
    procedure ViewAllClick(Sender: TObject);

  private
    { private declarations }
    lastState: TWindowState;
    lastTrayIconClick: qword;
  public
    { public declarations }
    oldListViewWindowProc: TWndMethod;

    searcherForm: TbookSearchFrm;

    BookList: TBookListView;
    booklistClickShift: TShiftState;
    booklistClickX: Integer;
    historyEditable, historyEditableAutostart: Boolean;


    procedure BookListDblClick(Sender: TObject);
    procedure BookListEditingDone(Sender: TObject);
    procedure BookListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure setPanelText(panel: TStatusPanel; atext: string);
    procedure ThreadDone(Sender: TObject);
    procedure RefreshListView; //Zentrale Anzeige Funktion!
    procedure refreshShellIntegration;
    procedure setSymbolAppearance(showStatus: integer); //0: text and icons 1=only text 2=only icons
    procedure refreshAccountGUIElements();
    //procedure WndProc(var TheMessage : TLMessage); override;
    function menuItem2AssociatedAccount(mi: TMenuItem): TCustomAccountAccess;

    function IsShortcut(var Message: TLMKey): boolean; override;


    procedure showVidelibri(var m:lcltype.TMsg); message LM_SHOW_VIDELIBRI;
    procedure showMainwindow;
  end;



var
  mainForm: TmainForm;
const SB_PANEL_COUNT=2;

procedure sendMailReports();


implementation

{ TmainForm }
uses math,options,debuglogviewer, newaccountwizard_u,bbdebugtools,bibtexexport,booklistreader{$IFDEF WIN32},windows{$ENDIF},Clipbrd,bbutils,androidutils,libraryaccesstester,duplicateremover,xqueryform,bookproperties,LazUTF8;

resourcestring
  rsSearchBarTitle = 'Ausleihensuche:';
  rsSearchBarNext = '&Vorwärts';
  rsSearchBarPrev = '&Rückwärts';
  rsSearchBarAll = 'alle Felder';
  rsSearchBarAuthorTitle = 'Verfasser/Titel';
  rsUpdateComplete = 'Das Update wurde installiert.%sDie installierte Version ist nun Videlibri %s';
  rsBookCount = 'Medien: %d';
  rsCharged = 'Offene Gebühren: %s';
  rsAccountDisabled = 'dieses Konto ist deaktiviert und der Ausleihstatus ist folgedessen unbekannt';
  rsTNAMenuLimit = '  Nächste Abgabefrist: %s';
  rsTNAHintLimitCheck = '  **Nächste Abgabefrist: %s**%s  Letzte Aktualisierung: %s';
  rsBookCountSelected = 'Medien: %d/%d';
  rsMenuHelpWindows = 'Fensterbeschreibung';
  rsMenuFAQ = 'Fragen und Antworten';
  rsAbout = 'Verwendete Fremdkomponenten: %s'+
            'Verwendete Entwicklungswerkzeuge: %s'+
            'Thanks to: %s'+
            'Die angezeigten Daten stammen von den jeweiligen Bibliotheken, %s. ';
  rsBlockedDuringRefresh = 'Diese Aktion kann nicht durchgeführt werden, solange Medien aktualisiert werden.';
  rsNeedAccountForRenewing = 'Sie müssen ein Konto angeben, um VideLibri für Verlängerungen benutzen zu können.%sWollen Sie eines eingeben?';
  rsCannotDeleteLend = 'Es sind momentan ausgeliehene Medien markiert, es können aber nur abgegebene Medien aus der History gelöscht '
    +'werden%sBitte demarkieren Sie sie.';
  rsDelete = 'Löschen';
  rsDeleteConfirm = 'Sind Sie sicher, dass Sie %s Medium/Medien löschen wollen?';
  rsSelectedBookMissing = 'Das markierte Buch war nicht vorhanden.%sDas ist eigentlich unmöglich, am besten starten Sie das Programm neu.';
  rsLastRefreshLong = 'Die angezeigten Daten der Konten wurden zu folgenden Zeitpunkten%sdas letztemal aktualisiert:';
  rsMailReportDate = 'Daten von: ';
  rsMailReportTitle = 'Videlibri Bücherreport vom %s';
  rsMailReportEmpty = 'Keine Bücher registriert';
  rsMailReportSubectNotRenewable = 'Nächste Frist: %s(NICHT verlängerbar) | %s';
  rsMailReportSubject = 'Nächste Frist: %s | %s';
  rsEditHistoryConfirm = 'Sollen die Eigenschaften dieses Buches editiert werden?';
  rsEditHistoryNoHistory = 'Nur abgebenen Bücher können editiert werden.';


function sendMailReportCompare(Item1, Item2: Pointer): Integer;
var book1,book2: TBook;
begin
  book1:=tbook(item1);
  book2:=tbook(item2);
  if book1.dueDate<book2.dueDate then exit(-1);
  if book1.dueDate>book2.dueDate then  exit(1);
  if (book1.status in BOOK_NOT_EXTENDABLE) and (book2.status in BOOK_EXTENDABLE) then exit(-1);
  if (book1.status in BOOK_EXTENDABLE) and (book2.status in BOOK_NOT_EXTENDABLE) then  exit(1);
  if book1.id < book2.id then exit(-1);
  if book1.id > book2.id then exit(1);
  exit(0);
end;

procedure sendMailReports();
function mailDate(d: integer):string;
begin
  result := DateToSimpleStr(d);
  if DateToPrettyStr(d) <> result then
    result += ' ('+DateToPrettyStr(d)+')';
end;

var prog: string;
 i: Integer;
 tmpbl: TBookList;
 usedaccounts: TFPList;
 accountsToSend: TStringArray;
 count: LongInt;
 receiver: String;
 interval: LongInt;
 j: Integer;
 k: Integer;
 report: String;
 week: LongInt;
 proc: TProcess;
 subject: String;

begin
  prog := userConfig.ReadString('Mail', 'Sendmail', 'sendmail -i -f "$from" $to');
  count := userConfig.ReadInteger('Mail', 'Reportcount', 0);
  tmpbl:=TBookList.Create;
  usedaccounts:=TFPList.Create;
  for i:=0 to count-1 do begin
    receiver :=userConfig.ReadString('Mailreport'+IntToStr(i), 'To', '');
    accountsToSend :=strSplit(userConfig.ReadString('Mailreport'+IntToStr(i), 'Accounts',  ''),',');
    interval := userConfig.ReadInteger('Mailreport'+IntToStr(i), 'Interval', 1);
    if userConfig.ReadInteger('Mailreport'+IntToStr(i), 'Lastsent', 0) + interval > currentDate then continue;

    tmpbl.clear;
    for k:=0 to accounts.Count-1 do
      for j:= 0 to high(accountsToSend) do
        if strContains((accounts[k]).getUser(), strTrim(accountsToSend[j])) or
           strContains((accounts[k]).prettyName, strTrim(accountsToSend[j]))  then begin
          tmpbl.addList((accounts[k]).books.current);
          usedaccounts.add(accounts[k]);
        end;

    tmpbl.Sort(@sendMailReportCompare);

    subject:= Format(rsMailReportTitle, [DateToSimpleStr(currentDate)]);
    report := subject + #13#10#13#10;
    if tmpbl.count = 0 then begin
      report += rsMailReportEmpty;
      report += #13#10;
    end else begin
      if (tmpbl.nextLimitDate(false) <= tmpbl.nextLimitDate()) or (tmpbl.nextLimitDate(false) < currentDate + 3) then
        subject := Format(rsMailReportSubectNotRenewable, [mailDate(tmpbl.nextLimitDate()), subject])
       else
        subject := Format(rsMailReportSubject, [mailDate(tmpbl.nextLimitDate()), subject]);

      week := dateToWeek(tmpbl.books[0].dueDate);
      for j:=0 to tmpbl.Count-1 do begin
        if dateToWeek(tmpbl.books[j].dueDate) <> week then begin
          week := dateToWeek(tmpbl.books[j].dueDate);
          report += #13#10#13#10;
        end;
        if tmpbl.books[j].owningAccount <> nil then report += tmpbl.books[j].owningAccount.prettyName + ': ';
        report +=  tmpbl.books[j].toSimpleString() + ':  ' + BookStatusToStr(tmpbl.books[j]) +  '  =>  ' +  mailDate(tmpbl.books[j].dueDate);
        report += #13#10;
      end;
    end;

    report += #13#10 + rsMailReportDate;
    for j:=0 to usedaccounts.Count-1 do
      with TCustomAccountAccess(usedaccounts[j]) do begin
        report+=#13#10'        '+prettyName+': '+DateToPrettyStr(lastCheckDate);
        if not enabled then report+='('+rsDisabled+')';
      end;

    report+=#13#10#13#10+'.'+#13#10#13#10;

    log(report);

    proc := TProcess.Create(nil);
    proc.Options := [poUsePipes];
    prog :=StringReplace(prog, '$from', ' Videlibri <benito@benibela.de>', [rfReplaceAll, rfIgnoreCase]);
    prog := StringReplace(prog, '$to', receiver, [rfReplaceAll,rfIgnoreCase]);
    prog := StringReplace(prog, '$subject', subject, [rfReplaceAll,rfIgnoreCase]);
    proc.CommandLine := prog;
    proc.Execute;
    proc.Input.WriteBuffer(report[1], length(report));
    proc.CloseInput;
    while proc.Running do begin
      Application.ProcessMessages;
      Sleep(1);
    end;
    userConfig.WriteInteger('Mailreport'+IntToStr(i), 'Lastsent', currentDate);
  end;
  usedaccounts.free;
  tmpbl.free;
end;


procedure TmainForm.FormCreate(Sender: TObject);
//const IMAGE_FILES:array[0..2] of string=('refresh','homepages','options');

//const defaultorder:array[0..9] of longint=(0,1,2,3,BL_BOOK_COLUMNS_YEAR_ID,4,5,6,7,9);

var i:integer;
    tempItem:TMenuItem;
    stream: TStringAsMemoryStream;
    po: TPOFile;
    libsAtLoc: TStringArray;
    tempItem2: TMenuItem;
    j: Integer;
    tempItem3: TMenuItem;
    state: String;
    loc: String;
    s: String;
begin
  if logging then log('FormCreate started');


  {$ifndef android}
  ToolBar1.Visible:=true;
  TrayIcon1.Visible:=true;
  StatusBar1.Visible:=true;
  bookMenu.Visible:=true;
  extraMenu.Visible:=true;
  viewMenu.Visible:=true;
  helpMenu.Visible:=true;
  {$else}
  //sleep(8000); for gdb
  StatusBar1.Visible:=false;
  androidActivationTimer.Enabled:=true;
  EnsureTrayIconTimer.Enabled:=false;
  {bookMenu.Visible:=false;
  extraMenu.Visible:=false;
  viewMenu.Visible:=false;
  helpMenu.Visible:=false;}
  {$endif} //turning to false on android also works


  accounts.OnAccountAdd:=@accountAddedChanged;

  stream := TStringAsMemoryStream.Create(assetFileAsString('lclstrconsts.de.po'));
  po := TPOFile.Create(stream);
  TranslateUnitResourceStrings('LCLStrConsts', po);
  po.free;
  stream.free;

  for i := 0 to high(bookproperties.translationMap) do
    bookproperties.translationMap[i].defaultWidth := MathRound(bookproperties.translationMap[i].defaultWidth * guiScaleFactor);

  BookList:=TBookListView.create(self,true);

  setSymbolAppearance(userConfig.ReadInteger('appearance','symbols',0));


  tempItem:=TMenuItem.Create(libraryList);
  for state in libraryManager.enumerateCountryStates() do begin
    tempItem := TMenuItem.Create(libraryList);
    if (state <> '-') and (state <> '- - -') then tempItem.Caption:=state
    else tempItem.Caption:='<eigene>';
    for loc in libraryManager.enumerateLocations(state) do begin
      tempItem2 := TMenuItem.Create(tempItem);
      if loc <> '-' then tempItem2.Caption := loc //if the name is '-' it is considered a separator and crashes
      else tempItem2.Caption := '<alle>';
      libsAtLoc := libraryManager.enumeratePrettyLongNames(loc);
      for j := 0 to high(libsAtLoc) do begin
        tempItem3 := TMenuItem.Create(tempItem2);
        if libsAtLoc[j] <> '-' then tempItem3.Caption:= libsAtLoc[j]
        else tempItem3.Caption:= '<Bücherei>';
        tempItem3.Tag:=j;
        tempItem3.OnClick:=@LibraryHomepageClick;
        tempItem2.Add(tempItem3);
      end;
      tempItem.Add(tempItem2);
    end;
    libraryList.Items.Add(tempItem);
  end;

  s := userConfig.ReadString('appearance', 'groupingProperty','');
  for i := 1 to high(groupingPropertyNames) do begin
    tempItem := TMenuItem.Create(groupingItem);
    tempItem.Caption := groupingPropertyNames[i];
    tempItem.GroupIndex := 3;
    tempItem.RadioItem:=true;
    tempItem.Tag := i;
    tempItem.OnClick := @MenuItem9Click;
    if groupingPropertyMap[i] = s then tempItem.Checked := true;
    groupingItem.add(tempItem);
  end;
  libraryList.items[0].Visible:=false;//place holder

  refreshAccountGUIElements();

  booklist.addDefaultColumns;
  BookList.Parent:=self;
  BookList.BackGroundColor:=colorOK;
  BookList.Options:=BookList.Options-[tlvoStriped]+[tlvoMultiSelect,tlvoSorted];
  BookList.PopupMenu:=bookPopupMenu;
   BookList.OnSelect:=@BookListSelectItem;
  BookList.SortColumn:=booklist.getPropertyColumnIndex('dueDate');
  BookList.OnUserSortItemsEvent:=@BookListUserSortItemsEvent;
  BookList.OnDblClick:=@BookListDblClick;
  booklist.OnMouseDown:=@BookListMouseDown;
 // BookList.TabOrder:=0;

  BookList.deserializeColumnOrder(userConfig.ReadString('BookList','ColumnOrder',''));
  BookList.deserializeColumnWidths(userConfig.ReadString('BookList','ColumnWidths',''));
  BookList.deserializeColumnVisibility(userConfig.ReadString('BookList','ColumnVisibility',''));
  BookList.createUserColumnVisibilityPopupMenu();
  {$ifndef android}
  BookList.createSearchBar();
  BookList.SearchBar.Caption:=rsSearchBarTitle;
  BookList.SearchBar.SearchForwardText:=rsSearchBarNext;
  BookList.SearchBar.SearchBackwardText:=rsSearchBarPrev;
  BookList.SearchBar.SearchLocations[0]:=rsSearchBarAll;
  BookList.SearchBar.SearchLocations.InsertObject(0, rsSearchBarAuthorTitle, tobject(ptruint(1 shl booklist.getPropertyColumnIndex('author')) or ptruint(1 shl booklist.getPropertyColumnIndex('title'))));
  {$endif}
  //TODO Iconoptimize
  //TODO Öffnungszeiten

  left:=userConfig.ReadInteger('window','left',left);
  top:=userConfig.ReadInteger('window','top',top);
  width:=userConfig.ReadInteger('window','width',width);
  height:=userConfig.ReadInteger('window','height',height);
  if left+width>screen.width then width:=screen.width-left;
  if top+height>screen.height then height:=screen.height-top;


  lastState:=WindowState;
  if lastState = wsMinimized then lastState:=wsNormal;

  caption:=appFullTitle;

  RefreshShellIntegration;

  if debugMode then MenuItemTester.Visible:=true;

  updateThreadConfig.OnPartialUpdated:=@updateThreadConfigPartialUpdated;
    //image1.Picture.LoadFromFile(programPath+'images'+DirectorySeparator+IMAGE_FILES[0]+'.bmp');
  if logging then log('FormCreate ende');
end;

procedure TmainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TmainForm.FormResize(Sender: TObject);
var i:integer;
    w:integer;
begin
  if logging then log('FormResize started');
  w:=StatusBar1.ClientWidth;
  for i:=0 to StatusBar1.Panels.count-1 do
    w:=w-StatusBar1.Panels[i].Width;
  StatusBar1.Panels[1].width:=w+StatusBar1.Panels[1].width;
          {
  if BookList.items.count=0 then begin
    if logging then log('FormResize ende (exit)');
    exit;
  end;
  for i:=0 to min(ListView1.TopItem.Index+ListView1.VisibleRowCount,
                                        ListView1.items.Count-1) do begin
    item:=ListView1.Items[i];
    ListView1.Canvas.brush.color:=getListItemColor(item);

    rec:=item.DisplayRect(drBounds);
    rec.left:=rec.right;
    rec.right:=listView1.clientWidth;
    ListView1.Canvas.FillRect(rec);
  end;                                               //}
  if logging then log('FormResize ende');
end;

procedure TmainForm.bookPopupMenuPopup(Sender: TObject);
var i: longint;
  extendableBooks: Integer;
  cancelableBooks: Integer;
  books: TBookList;

begin
{  extendThisBooks.Enabled:=(ListView1.SelCount=1) and
                  (PBook(ListView1.Selected.Data))^.lib.getLibrary().canModifySingleBooks;}
  books := BookList.SelectedBooks;
  extendableBooks := 0;
  cancelableBooks := 0;
  for i:=0 to books.Count-1 do
    with books[i] do begin
      if status in BOOK_EXTENDABLE then extendableBooks += 1
      else if (status in [bsOrdered,bsProvided]) and (cancelable <> tFalse) then cancelableBooks += 1;
    end;
  extendTheseBooks.Enabled:=(books.Count>=1) and (extendableBooks = books.Count);
  extendAdjacentBooks.Enabled:=books.Count=1;
  cancelTheseBooks.Enabled:=(books.Count>=1) and (cancelableBooks = books.Count);
  displayDetailsMI.Enabled:=books.Count=1;
  searchDetailsMI.Enabled:=books.Count=1;
  removeSelectedMI.Enabled:=books.Count>0;
  books.free;
end;

procedure TmainForm.BookListDblClick(Sender: TObject);
var
  recordItem: TTreeListRecordItem;
  col: LongInt;
begin
  if BookList.selCount<>1 then exit;
  if (ssShift in booklistClickShift) or (historyEditableAutostart) then begin
    if booklist.SelectedBook = nil then exit;
    if booklist.SelectedBook.lend then begin
      ShowMessage(rsEditHistoryNoHistory);
      exit;
    end;
    recordItem := booklist.Selected.GetRecordItemAtPos(BookList, booklistClickX);
    col := recordItem.Index;
    case booklist.properties[col] of
      'dueDate', 'issueDate', '?account': exit;
    end;
    if historyEditable or confirm(rsEditHistoryConfirm) then historyEditable := true
    else exit;
    historyEditableAutostart := true;
    booklist.OnEditingDone:=@BookListEditingDone;
    BookList.startEditing(recordItem);
  end else
    displayDetailsMIClick(displayDetailsMI);
end;

procedure TmainForm.BookListEditingDone(Sender: TObject);
var
  item: TTreeListRecordItem;
begin
  if (BookList.feditedRecordItem = nil) or (booklist.feditor = nil) then exit;
  if BookList.feditedRecordItem.Text = booklist.feditor.text then exit;
  if BookList.feditedRecordItem.Parent <> BookList.Selected then exit;
  item := BookList.feditedRecordItem;
  EnterCriticalSection(updateThreadConfig.libraryAccessSection);
  EnterCriticalSection(updateThreadConfig.libraryFileAccess);
  try
    booklist.SelectedBook.setProperty(booklist.properties[item.Index], booklist.feditor.Text);
    item.text := booklist.feditor.Text;
    (booklist.SelectedBook.owningAccount as TCustomAccountAccess).save();
  finally
    LeaveCriticalSection(updateThreadConfig.libraryFileAccess);
    LeaveCriticalSection(updateThreadConfig.libraryAccessSection);
  end;
end;

procedure TmainForm.androidActivationTimerTimer(Sender: TObject);
begin
  androidActivationTimer.OnTimer:=nil;
  androidActivationTimer.Enabled:=false;
  if OnActivate <> nil then OnActivate(nil);
end;

procedure TmainForm.accountsChanged(Sender: TObject);
begin
end;

procedure TmainForm.accountAddedChanged(acc: TCustomAccountAccess);
begin
  if optionForm <> nil then optionForm.addAccount(acc);
  refreshAccountGUIElements();
end;




procedure TmainForm.BookListUserSortItemsEvent(sender: TObject;
  var sortColumn: longint; var invertSorting: boolean);
begin
end;

procedure TmainForm.cancelTheseBooksClick(Sender: TObject);
var i:integer;
    books:TBookList;
begin
  books:=BookList.SelectedBooks;
  for i := books.Count - 1 downto 0 do
    if not (books[i].status in [bsOrdered,bsProvided]) then
      books.delete(i);
  cancelBooks(books);
  books.free;
end;

procedure TmainForm.dailyCheckThreadTimer(Sender: TObject);
//this will be called every 24h to ensure that videlibri also works if you
//never turn your computer off
begin
  if alertAboutBooksThatMustBeReturned then show
  else begin
    accountsRefreshedDate:=0;
    repeatedCheckTimer.Enabled:=true;
  end;
end;

procedure TmainForm.delayedCallTimer(Sender: TObject);
begin
  delayedCall.Enabled:=false;
 showErrorMessages();
end;

procedure TmainForm.EnsureTrayIconTimerTimer(Sender: TObject);
begin
  //the Lazarus tray icon is not reliable (at least on debian), so repeat until
  //it is actually shown
  if TrayIcon1.Visible then
    EnsureTrayIconTimer.Enabled:=false
  else
    TrayIcon1.Show;
end;

procedure TmainForm.extendAdjacentBooksClick(Sender: TObject);
begin
  if (BookList.Selected = nil) or (BookList.Selected.data.obj=nil) then exit;
  extendBooks(MaxLongint,tbook(BookList.Selected.data.obj).owningAccount as TCustomAccountAccess);
end;

procedure TmainForm.FormActivate(Sender: TObject);
begin
  if logging then log('FormActivate started');
  {$ifdef android}
  if sender <> nil then exit; //activate is not called on android. Call it from timer with nil. Check prevents double calling, when onActivate is fixed and called in possible later LCL versions
  {$endif}
  setPanelText(StatusBar1.Panels[3],{'Datum: '+}DateToSimpleStr(currentDate));
  if newVersionInstalled then
    ShowMessage(Format(rsUpdateComplete, [#13#10, FloatToStr(versionNumber/1000)]));
  onshow:=nil;
  OnActivate:=nil;
  windowstate:=twindowstate(userConfig.ReadInteger('window','state',integer(windowstate)));
  if accounts.count>0 then begin
    if userconfig.ReadInteger('BookList', 'Mode', 0) = 1 then ViewAllClick(ViewAll)
    else RefreshListView;
    if not needApplicationRestart then
      defaultAccountsRefresh;
  end;
  if not needApplicationRestart then
    if accounts.count=0 then begin
      if logging then log('No accounts. Show TnewAccountWizard');
      with TnewAccountWizard.Create(nil) do begin
        OnClose:=@newAccountWizardClosed;
        Show;
        if logging then log('wizard done');
      end;
    end;
  if logging then log('FormActivate ended');
  //InternetCheckConnection(FLAG_ICC_FORCE_CONNECTION) ping
    //InternetGetConnectedState (veraltet/get net
    {DFÜ:

    lRet = RasEnumConnections(tRASCONN, Len(tRASCONN), lConnCount)

    '// Geht davon aus, dass eine DFÜ Verbindung besteht,
    '// wenn die Anzahl der Verbindungen > 0:
    IsOnline = (lConnCount > 0)
    }
end;

procedure TmainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if windowstate<>wsMaximized then begin
    userConfig.WriteInteger('window','left',left);
    userConfig.WriteInteger('window','top',top);
    userConfig.WriteInteger('window','width',width);
    userConfig.WriteInteger('window','height',height);
  end;
  if windowstate<>wsMinimized then
    userConfig.writeinteger('window','state',integer(windowstate));

  if BookList <> nil then begin
    userConfig.WriteString('BookList','ColumnOrder',BookList.serializeColumnOrder);
    userConfig.WriteString('BookList','ColumnWidths',BookList.serializeColumnWidths);
    userConfig.WriteString('BookList','ColumnVisibility',BookList.serializeColumnVisibility);
  end;

  if ViewAll.Checked then userconfig.WriteInteger('BookList', 'Mode', 1)
  else userconfig.WriteInteger('BookList', 'Mode', 0);

  if searcherForm <> nil then searcherForm.saveDefaults;
  FreeAndNil(searcherForm);
  FreeAndNil(BookList);
end;

procedure TmainForm.BookListSelectItem(Sender: TObject; Item: TTreeListItem);
begin
  if BookList.SelCount=0 then setPanelText(StatusBar1.Panels[SB_PANEL_COUNT], format(rsBookCount, [BookList.bookCount]) )
  else setPanelText(StatusBar1.Panels[SB_PANEL_COUNT], format(rsBookCountSelected, [BookList.selCount, BookList.bookCount]));
end;

procedure TmainForm.FormShow(Sender: TObject);
begin
end;

procedure TmainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState=wsMinimized then hide
  else lastState := WindowState;
end;

procedure TmainForm.menuDuplicateCheckerClick(Sender: TObject);
var form: TduplicateForm;
begin
  form := TduplicateForm.Create(nil);
  form.ShowModal;
  form.free;
  RefreshListView;
end;

procedure TmainForm.MenuItem11Click(Sender: TObject);
begin
  if (BookList.SelectedBook = nil) or (booklist.selCount > 1) then exit;
  historyEditable := true; historyEditableAutostart := true;
  BookListDblClick(booklist);
end;

procedure TmainForm.MenuItemDebugLogClick(Sender: TObject);
begin
  TDebugLogForm.Create(Application).show;
end;

procedure TmainForm.MenuItemRenewClick(Sender: TObject);
var i:integer;
    books:TBookList;
begin
  books:=BookList.SelectedBooks;
  for i := books.Count - 1 downto 0 do
    if not (books[i].status in BOOK_EXTENDABLE) then
      books.delete(i);
  extendBooks(books);
  books.free;
end;

procedure showCHM(filename:string;contextID:longint; tocname:string);
  {$IFNDEF WIN32}
  function couldRun(s:string): boolean;
  var
    p: TProcess;
  begin
    p:=TProcess.Create(nil);
    try
    try
      p.CommandLine:=s;
      p.Execute;
      result:=true;
    finally
      p.free;
    end;
    except on e:EProcess do
      result:=false;
    end;
  end;
  {$ENDIF}

begin
  filename:='"'+assetPath+ filename+'"';
  {$IFDEF WIN32}
  WinExec(pchar('hh -mapid  '+IntToStr(contextID)+' '+filename),sw_shownormal); //TODO: use modern command like ShellExecute
  {$ELSE}
  if not couldRun('xchm -c '+inttostr(contextID)+' '+filename) then
    if not couldRun('kchmviewer --stoc "'+tocname+'" '+filename) then
      if not couldRun('chmsee '+filename) then
        if not couldRun('gnochm '+filename) then
          ShowMessage('Kein Programm zur Anzeige der Hilfe gefunden.'#13#10'Sie sollten entweder xchm, kchmviewer, chmsee oder gnochm installieren');

  {$ENDIF}
end;

procedure TmainForm.MenuItem14Click(Sender: TObject);
begin
  showCHM('videlibri.chm', 1000, rsMenuHelpWindows);
end;

procedure TmainForm.MenuItem15Click(Sender: TObject);
begin
  showCHM('videlibri.chm', 1001, rsMenuFAQ);
end;

procedure TmainForm.MenuItem16Click(Sender: TObject);
var invalid: SizeInt;
begin
  strLengthUtf8(oldErrorMessageString, invalid);
  if invalid > 0 then UTF8FixBroken(oldErrorMessageString);
  TsendBackErrorForm.openErrorWindow(oldErrorMessageString,IntToStr(versionNumber),'Videlibri');
end;

procedure TmainForm.MenuItem17Click(Sender: TObject);
{$IFDEF WIN32}
type tpopenAboutDialog = procedure (title,description,creditlist: pchar; modal: boolean; game: longint);stdcall;
var  openAboutDialog: tpopenAboutDialog;
     lib:thandle;
{$ENDIF}
var infotext: string;
begin
  infotext:=
  Format(rsAbout, ['LCL'#13#10'  FreePascal Runtime'#13#10'  TRegExpr ' +rsBy+ ' Andrey V. Sorokin'#13#10'  FLRE ' +rsBy+ ' Benjamin Rosseaux' + LineEnding,
                   'Lazarus'#13#10'  FreePascal'#13#10'  GIMP'#13#10'  HTML Help Workshop'#13#10'  The Regex Coach'+ LineEnding,
                   'Leonid Sokolov, Martin Kim Dung-Pham, Michael Volkmann'+ LineEnding,
                   'HBZ, Amazon, openlibrary.org ' +rsAnd+ ' buchhandel.de']);
  {$IFDEF WIN32}
  lib:=LoadLibrary('bbabout.dll');
  openAboutDialog:=tpopenAboutDialog(GetProcAddress(lib,'openAboutDialog'));
  if assigned(openAboutDialog) then begin
    openAboutDialog(pchar({'Videlibri '+FloatToStr(versionNumber/1000)}caption),'',pchar(Utf8ToAnsi(infotext)),true,1);
    FreeLibrary(lib);
    exit;
  end;
  FreeLibrary(lib);
  {$ENDIF}
  ShowMessage(caption+#13#10#13#10#13#10+infotext); //TODO: platform independen about dialog
end;

procedure TmainForm.MenuItem18Click(Sender: TObject);
begin
  callbacks. applicationUpdate(false);
end;

procedure TmainForm.MenuItem1Click(Sender: TObject);
var XMLExportForm:TXMLExportFrm;
begin
  XMLExportForm:=TXMLExportFrm.Create(nil);
  try
    XMLExportForm.ShowModal;
  finally
    XMLExportForm.free;
  end;
end;

procedure TmainForm.MenuItem9Click(Sender: TObject);
var
  i: PtrInt;
begin
  i := (sender as tcomponent).Tag;
  if (i >= 0)
     and (i < length(groupingPropertyMap))
     and (groupingPropertyMap[i] <> userConfig.ReadString('appearance', 'groupingProperty', '')) then begin
    userConfig.WriteString('appearance','groupingProperty', groupingPropertyMap[i]);
    RefreshListView;
  end;
end;

procedure TmainForm.MenuItemTesterClick(Sender: TObject);
begin
  TlibraryTesterForm.Create(Application).show;
end;

procedure TmainForm.MenuItem24Click(Sender: TObject);
var bibexportForm:TBibTexExportFrm;
begin
  bibexportForm:=TBibTexExportFrm.Create(nil);
  try
    bibexportForm.ShowModal;
  finally
    bibexportForm.free;
  end;
end;

procedure TmainForm.MenuItem25Click(Sender: TObject);
begin
  if searcherForm=nil then begin searcherForm:=TbookSearchFrm.Create(nil); refreshAccountGUIElements(); end;
  searcherForm.selectBookToReSearch(nil);
  searcherForm.Show;
  searcherForm.saveDefaults;

end;

procedure TmainForm.MenuItem26Click(Sender: TObject);
begin
  showMainwindow;
end;

procedure TmainForm.MenuItem27Click(Sender: TObject);
begin
  updateAccountBookData(nil,false,false,false);
end;

procedure TmainForm.MenuItem28Click(Sender: TObject);
begin
  updateAccountBookData(nil,false,false,true);
end;

procedure TmainForm.MenuItem30Click(Sender: TObject);
begin
  close
end;

procedure TmainForm.MenuItem31Click(Sender: TObject);
var t: string;
 i: Integer;
 books: TBookList;
 copyLimits: Boolean;
begin
  if (booklist.feditor <> nil) and (booklist.feditedRecordItem <> nil) then begin
    booklist.feditor.CopyToClipboard;
    exit;
  end;
  books := BookList.SelectedBooks;
  copyLimits := userConfig.ReadBool('user','copy-limit',false);
  t := '';
  for i:=0 to Books.count-1 do
    if (books[i].owningAccount<>nil) then
      if copyLimits then
        t += books[i].owningAccount.prettyName + ':  ' + books[i].toLimitString() + LineEnding
       else
         t += books[i].author + ': ' + books[i].title + LineEnding;

  clipboard.astext := t;
  books.free;
end;

procedure TmainForm.menuXqueryClick(Sender: TObject);
var form: Txqueryfrm;
begin
  form := Txqueryfrm.Create(nil);
  form.Show;
end;

procedure TmainForm.newAccountWizardClosed(Sender: TObject; var CloseAction: TCloseAction);
begin
  if accounts.count = 0 then
    if Application.MessageBox(pchar(Format(rsNeedAccountForRenewing, [#13#10])), 'Videlibri', MB_YESNO or MB_ICONQUESTION)=mrYes then begin
      CloseAction:=caNone;
      exit;
  end;
  CloseAction:=caFree;
end;

procedure TmainForm.removeSelectedMIClick(Sender: TObject);
var i:longint;
    accountsToSave: tlist;
    books: TBookList;
begin
  if updateThreadConfig.updateThreadsRunning>0 then begin
    ShowMessage(rsBlockedDuringRefresh) ;
    exit;
  end;
  books := BookList.SelectedBooks;
  try
  for i:=0 to books.Count-1 do
    if books[i].lend then begin
      ShowMessage(Format(rsCannotDeleteLend, [#13#10]));
      exit;
    end;
  if books.count = 0 then exit;
  if MessageDlg(rsDelete,
                  Format(rsDeleteConfirm, [IntToStr(books.count)]),
                  mtConfirmation ,[mbYes,mbNo],0)<>mrYes then exit;

  accountsToSave:=tlist.Create;
  try
    for i:=0 to books.count-1 do begin
        if accountsToSave.IndexOf(books[i].owningAccount)<0 then accountsToSave.Add(books[i].owningAccount);
        if TCustomAccountAccess(books[i].owningAccount).books.old.Remove(books[i]) < 0 then begin
          ShowMessage(Format(rsSelectedBookMissing, [#13#10]));
          exit;
        end;
      end;
    for i:=0 to accountsToSave.Count-1 do
      TCustomAccountAccess(accountsToSave[i]).save();
  RefreshListView;
  finally
    accountsToSave.Free;
  end;
  finally
    books.free;
  end;
end;

procedure TmainForm.displayDetailsMIClick(Sender: TObject);
begin
  if (BookList.Selected = nil) or (BookList.Selected.data.obj=nil) then exit;
  historyEditableAutostart := false;
  if searcherForm=nil then begin searcherForm:=TbookSearchFrm.Create(nil); searcherForm.loadDefaults; refreshAccountGUIElements(); end;
  searcherForm.selectBookToReSearch(tbook(BookList.Selected.data.obj));
  if TComponent(sender).tag<>1 then searcherForm.startSearch.Click;
  searcherForm.Show;
end;

procedure TmainForm.repeatedCheckTimerTimer(Sender: TObject);
//this tries to update the books every 5min until a connection to the library
//could actually established
var internet: TInternetAccess;
begin
  if accountsRefreshedDate = currentDate then begin
    repeatedCheckTimer.Enabled:=false;
    exit;
  end;

  internet:=defaultInternetAccessClass.create;
  try
  try
    if internet.existsConnection then begin
      if logging then log('repeatedCheckTimer: internet connection exists');
      defaultAccountsRefresh;
      if logging then log('repeatedCheckTimer: called defaultAccountsRefresh');
    end;
  finally
    internet.free;
    if logging then log('internet freed');
  end;
  except
  end;
end;

procedure TmainForm.searchTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TmainForm.TrayIcon1Click(Sender: TObject);
begin
  if GetTickCount64 - lastTrayIconClick < 250 then exit;
  lastTrayIconClick:=GetTickCount64;
  if Visible then begin
    {$ifndef win32}Hide;{$endif}
    exit;
  end;
  showMainwindow;
//  TrayIcon1DblClick(sender);
  //{$ifdef win32}TrayIcon1DblClick(sender); exit;{$endif}
  //TrayIconClick.Enabled:=true;
end;

procedure TmainForm.TrayIcon1DblClick(Sender: TObject);
begin
  //showMainwindow
end;

procedure TmainForm.TrayIconClickTimer(Sender: TObject);
begin
  if not TrayIconClick.Enabled then exit;
  TrayIconClick.Enabled:=false;
  if Visible then exit;
  TrayIcon1.PopUpMenu.PopUp;
end;

procedure TmainForm.TreeListView1Click(Sender: TObject);
begin

end;

procedure TmainForm.updateThreadConfigPartialUpdated(Sender: TObject);
begin
  if (updateThreadConfig.listUpdateThreadsRunning<=0) and (mainform<>nil) and (mainForm.visible) then
    TThread.Synchronize(nil, @mainForm.RefreshListView);
end;


procedure TmainForm.UserExtendMenuClick(Sender: TObject);
var limitTime: longint;
begin
  limitTime:=(sender as tmenuItem).Parent.tag;
  extendBooks(limitTime+currentDate,menuItem2AssociatedAccount(tmenuitem(sender)));
end;

procedure TmainForm.MenuItem3Click(Sender: TObject);
begin
  close;
end;

procedure TmainForm.MenuItem6Click(Sender: TObject);
var statistikForm:TstatistikForm;
begin
  statistikForm:=TstatistikForm.Create(nil);
  statistikForm.ShowModal;
  statistikForm.free;
end;

procedure TmainForm.MenuItem8Click(Sender: TObject);
begin
  {$ifndef android}
  BookList.SearchBar.Show;
  BookList.SearchBar.SetFocus;
  {$endif}
end;

procedure TmainForm.searchCloseClick(Sender: TObject);
begin
end;

procedure TmainForm.searchTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TmainForm.searchUpClick(Sender: TObject);
begin
end;

procedure TmainForm.showAccount(Sender: TObject);
var i:integer;
begin
  assert(sender is TMenuItem);
  if (sender =showAllAccounts) or (sender=showNoAccounts) then begin
    for i:=0 to viewMenu.Count-1 do
      if (viewMenu.Items[i].tag >= 1) then
        viewMenu.items[i].Checked:=sender=showAllAccounts;
  end else if GetKeyState(VK_SHIFT)and $8000<>0 then begin
      for i:=0 to viewMenu.Count-1 do
        if (viewMenu.Items[i].tag >= 1) then
          viewMenu.items[i].Checked:=viewMenu.items[i]=sender;
  end else
    TMenuItem(sender).Checked:=not TMenuItem(sender).Checked;
  RefreshListView;
end;

procedure TmainForm.refreshAccountClick(Sender: TObject);
begin
  updateAccountBookData(menuItem2AssociatedAccount((sender as tmenuitem)),false,false,false);
end;

procedure TmainForm.LibraryHomepageClick(Sender: TObject);
var baseURL:string;
    lib:TLibrary;
begin
  if tcontrol(sender).Tag=-1 then begin
    baseURL:='http://www.digibib.net/Digibib?SERVICE=SESSION&SUBSERVICE=GUESTLOGIN&LOCATION=DUEBIB&LANGUAGE=de';
  end else begin
    lib:=libraryManager.getLibraryFromEnumeration(TMenuItem(sender).Parent.Caption, tcontrol(sender).Tag);
    baseURL:=lib.homepageCatalogue;
    //if not lib.allowHomepageNavigation then extraParams:=extraParams+' /no-navigation';
    //if lib.bestHomepageWidth>0 then extraParams:=extraParams+' /pagewidth='+InttoStr(lib.bestHomepageWidth);
    //if lib.bestHomepageHeight>0 then extraParams:=extraParams+' /pageheight='+InttoStr(lib.bestHomepageHeight);
  end;
  OpenURL(baseURL);//,'/title="'+title+'" /configfile="'+userPath+'browserconfig.ini" /windowsection='+id+' '+extraParams);
  //ShowMessage(programPath+'simpleBrowser /title="'+TMenuItem(sender).Caption+'" /site="'+baseURL+'" /configfile="'+userPath+'browerconfig.ini" /windowsection='+id);
end;

procedure TmainForm.StatusBar1DblClick(Sender: TObject);
var pos: TPoint;
    i,j:integer;
    s:string;
begin
  GetCursorPos(pos{%H-});
  pos:=StatusBar1.ScreenToClient(pos);
  for i:=0 to StatusBar1. Panels.Count-1 do
    if pos.x<=statusbar1.panels.items[i].Width then begin
      case i of
        0: begin
             s:=Format(rsLastRefreshLong, [#13#10]);
             for j:=0 to accounts.count-1 do
               with (accounts[j]) do begin
                 s:=s+#13#10'        '+prettyName+': '+DateToPrettyStr(lastCheckDate);
                 if not enabled then s+=' ('+rsDisabled+')';
               end;
             ShowMessage(s);
           end;
      end;
    end else dec(pos.x,statusbar1.panels.items[i].Width);
end;

procedure TmainForm.Timer1Timer(Sender: TObject);
begin
end;

procedure TmainForm.ShowOptionsClick(Sender: TObject);
var optionsForm:ToptionForm;
begin
  optionsForm:=ToptionForm.Create(nil);
  try
     optionsForm.ShowModal;
  finally
     optionsForm.free;
  end;
end;

procedure TmainForm.btnRefreshClick(Sender: TObject);
begin
  updateAccountBookData(nil,false,false,false);
end;

procedure TmainForm.ToolButton2ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled:=true;
end;

procedure TmainForm.ToolButton3Click(Sender: TObject);
var pos: TPoint;
begin
  pos.x:=0;
  pos.y:=ToolButton3.Height;
  pos:=ToolButton3.ClientToScreen(pos);
  libraryList.PopUp(pos.x,pos.y);
end;



procedure TmainForm.ViewAllClick(Sender: TObject);
begin
  (sender as tmenuitem).Checked:=true;
  if sender=ViewOld then BookList.BackGroundColor:=colorOld
  else BookList.BackGroundColor:=colorOK;
  RefreshListView;
end;

procedure TmainForm.setPanelText(panel: TStatusPanel; atext: string);
var textWidth: longint;
begin
  panel.Text:=atext;
  textWidth:=Canvas.TextWidth(atext)+10;
  if Canvas.HandleAllocated and (textWidth>panel.Width) then begin
    panel.Width:=textWidth+10;
    FormResize(self); //update scrollbar panel width
  end;
end;

procedure TmainForm.BookListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  booklistClickShift := shift;
  booklistClickX := X;
end;

procedure TmainForm.RefreshListView;
var i:integer;
    criticalSessionUsed,oldList,currentList:boolean;
    account: TCustomAccountAccess;
    maxcharges:currency;
    tempitem: TTreeListItem;
begin
  if logging then log('RefreshListView started');
  lastCheck:=currentDate;
  oldList:=ViewAll.Checked or ViewOld.Checked;
  currentList:=ViewAll.Checked or ViewCurrent.Checked;
  criticalSessionUsed:=false;
  if updateThreadConfig.updateThreadsRunning>0 then begin
    system.EnterCriticalSection(updateThreadConfig.libraryAccessSection);
    criticalSessionUsed:=true;
  end;
  try
  BookList.groupingProperty := userConfig.ReadString('appearance', 'groupingProperty','');
  BookList.BeginUpdate;
  BookList.clear;
  for i:=0 to viewMenu.Count-1 do begin
    if (viewMenu.Items[i].tag = -1) then continue;
    if not viewMenu.Items[i].Checked then continue;
    assert(viewMenu.Items[i].tag>=1);
    assert(viewMenu.Items[i].tag<=accounts.Count);
    account:=(accounts[viewMenu.Items[i].tag-1]);
    if currentList then begin
      bookList.addBookList(account.books.current, viewMenu.Items[i].Caption);
      lastCheck:=min(lastCheck,account.books.current.lastCheck);
    end;
    if oldList then bookList.addBookList(account.books.old, viewMenu.Items[i].Caption);
  end;
  BookList.Sort;

  maxcharges:=0;
  for i:=0 to accounts.Count-1 do
    maxcharges:=maxcharges+(accounts[i]).charges;

  if maxcharges>0 then begin
    StatusBar1.Panels[1].text:=Format(rsCharged, [floattostr(maxcharges) + '€ (']);
    for i:=0 to accounts.Count-1 do
        if accounts[i].charges>0 then
           StatusBar1.Panels[1].text:=StatusBar1.Panels[1].text+
              accounts[i].prettyName+': '+
              FloatToStr(accounts[i].charges) +'€ ';
    setPanelText(StatusBar1.Panels[1],StatusBar1.Panels[1].text+')');
  end else setPanelText(StatusBar1.Panels[1],'');
  setPanelText(StatusBar1.Panels[SB_PANEL_COUNT], format(rsBookCount, [BookList.bookCount]));


  if updateThreadConfig.updateThreadsRunning<=0 then begin
    setPanelText(StatusBar1.Panels[0], Format(rsLastRenewDate, [DateToPrettyStrGrammarPast(lastCheck)]));

  end;

  RefreshShellIntegration();

  BookList.EndUpdate;


  for i:=0 to viewMenu.Count-1 do begin
    if (viewMenu.Items[i].tag = 0) or (viewMenu.Items[i].tag = -1) then continue;
    if menuItem2AssociatedAccount(viewMenu.Items[i]).enabled then continue;


    BookList.BeginUpdate;
    tempitem := BookList.items.Add;
    with tempitem do begin
      text:='ACHTUNG';
      RecordItemsText[booklist.getPropertyColumnIndex('author')] := 'VideLibri';
      RecordItemsText[booklist.getPropertyColumnIndex('title')] := rsAccountDisabled;
      RecordItemsText[booklist.getPropertyColumnIndex('?account')] := menuItem2AssociatedAccount(viewMenu.Items[i]).prettyName;
      BookList.getAdditionalBookData(tempitem).colorState:=bcsTimeNear;
      Tag:=0;
    end;
    BookList.EndUpdate;
  end;
  ;



  finally
    if criticalSessionUsed then
      system.LeaveCriticalSection(updateThreadConfig.libraryAccessSection);
  end;

  if logging then log('RefreshListView ended');
end;

procedure TmainForm.refreshShellIntegration;
var
  iconStream: TStringAsMemoryStream;
  iconName: String;
begin
  if logging then log('refreshShellIntegration started');
  MenuItem29.Caption:=Format(rsTNAMenuLimit, [nextLimitStr]);
  TrayIcon1.Hint:='Videlibri'#13#10+Format(rsTNAHintLimitCheck, [nextLimitStr, #13#10, DateToPrettyStr(lastCheck)]);
  iconName := getTNAIconBaseFileName();
  if logging then log('Display icon: ' + iconName);
  iconStream := TStringAsMemoryStream.Create(assetFileAsString(iconName));
  if logging then log('Icon size: ' + IntToStr(iconStream.Size));
  try
    TrayIcon1.Icon.LoadFromStream(iconStream); iconStream.Position := 0;
    icon.LoadFromStream(iconStream); iconStream.Position := 0;
    Application.icon.LoadFromStream(iconStream); iconStream.Position := 0;
  except
    on e: Exception do
      if logging then log('Icon displaying failed: ' + e.Message);
  end;
  iconStream.Free;
  if logging then log('refreshShellIntegration ended');
end;


procedure TmainForm.setSymbolAppearance(showStatus: integer);
var bheight: integer;
begin
  bheight:=0;
  if showStatus and 2=0 then bheight:=bHeight+BookList.RowHeight;
  ToolBar1.ShowCaptions:=showStatus and 2=0;
  if showStatus and 1=0 then begin
    bheight:=bHeight+52;
    ToolBar1.Images:=ImageList1;
  end else ToolBar1.Images:=nil;
  ToolBar1.Height:= bheight+2;
  ToolBar1.ButtonHeight:=bheight;
end;


procedure TmainForm.refreshAccountGUIElements();

  function newItem(owner: TComponent;onClick:TNotifyEvent;id:longint):TMenuItem;
  begin
    newItem:=TMenuItem.Create(owner);
    newItem.Caption:=(accounts[id]).prettyName;
    newItem.tag:=id+1; //add one to distinguish valid values from default tag 0
    newItem.OnClick:=onclick;
  end;
  procedure addToMenuItem(item: TMenuItem;onClick:TNotifyEvent;id:longint);
  begin
    item.add(newItem(item,onclick,id));
  end;

  procedure clearMenuItem(item:TMenuItem);
  var i:integer;
  begin
    for i:=item.Count-1 downto 1 do
      if item.items[i].tag>=1 then item.Delete(i)
  end;

var temp:TMenuItem;
    i:integer;
    j: Integer;
begin
  //delete old
  clearMenuItem(accountListMenuItem);
  clearMenuItem(viewMenu);
  clearMenuItem(extendMenuList1);
  for i:=extendMenuList2_.count-1 downto 0 do
    clearMenuItem(extendMenuList2_[i]);
  for i:=accountListMenu.items.count-1 downto 0 do
    if accountListMenu.items[i].tag>=1 then
      accountListMenu.items.Delete(i);

  //add new
  for i:=0 to accounts.Count-1 do begin
    accountListMenu.Items.Add(newItem(accountListMenu,@refreshAccountClick,i));
    accountListMenuItem.Add(newItem(accountListMenu,@refreshAccountClick,i));
    temp:=newItem(accountListMenu,@showAccount,i);
    temp.Checked:=true;
    viewMenu.Add(temp);
    addToMenuItem(extendMenuList1,@UserExtendMenuClick,i);
    for j:=0 to extendMenuList2_.Count-1 do
      addToMenuItem(extendMenuList2_[j],@UserExtendMenuClick,i);
  end;

  if searcherForm <> nil then begin
    searcherForm.saveToAccountMenu.Items.Clear;
    searcherForm.orderForAccountMenu.Items.Clear;
    for i:=0 to accounts.Count-1 do begin
      temp := newItem(searcherForm.saveToAccountMenu,@searcherForm.changeDefaultSaveToAccount,i);
      temp.GroupIndex:=124;
      temp.RadioItem:=true;
      searcherForm.saveToAccountMenu.Items.Add(temp);
      if (i = 0) or (searcherForm.saveToDefaultAccountID = accounts.Strings[i]) then temp.Checked:=true;

      temp := newItem(searcherForm.orderForAccountMenu,@searcherForm.changeDefaultOrderForAccount,i);
      temp.GroupIndex:=124;
      temp.RadioItem:=true;
      searcherForm.orderForAccountMenu.Items.Add(temp);
      if (i = 0) or (searcherForm.orderForDefaultAccountID = accounts.Strings[i]) then temp.Checked:=true;
    end;
  end;
end;


function TmainForm.menuItem2AssociatedAccount(mi: TMenuItem
  ): TCustomAccountAccess;
begin
  if (mi.tag<=0) or (mi.tag>accounts.Count) then
    raise exception.Create('Ungültiges Konto angegeben');
  result:=(accounts[mi.tag-1]);
end;

function TmainForm.IsShortcut(var Message: TLMKey): boolean;
begin
  result := false;
  if not Active then exit;
  if ActiveControl is TEdit then exit;
  result := inherited IsShortcut(Message);
end;


procedure TmainForm.showVidelibri(var m:lcltype.TMsg);
begin
  {$ifdef win32}
  showMainwindow
  {$endif}
end;

procedure TmainForm.showMainwindow;
begin
  // TrayIconClick.Enabled:=false;
  //TODO: why doesn't this work if it is maximized?????
  application.BringToFront;
  if Enabled then begin
 //   WindowState:=lastState;
    {$Ifdef win32}windowstate:=wsNormal;{$endif}
    show;
    BringToFront;
  end;
  TrayIconClick.Enabled:=false;
end;

procedure TmainForm.ThreadDone(Sender: TObject);
//called in the main thread
begin
  libraryAccess.threadDone.ThreadDone(Sender); //self=nil !!!
end;


initialization
  {$I bookwatchmain.lrs}
end.
