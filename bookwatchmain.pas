{ChangeLog:
  2007-04-29  Icons werden unter Win9x angezeigt (durch Update auf lcl-rev. 11027)
              Dialogbuttons sind auf deutsch
}
unit bookWatchMain;
//TODO2: Buchhintergrund beim Scrollen um 1 verschoben?
//TODO: Hilfedatei vervollständigen

{$mode objfpc}{$H+}



interface
                                      
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, libraryParser, internetAccess, ComCtrls, Menus, lmessages, ExtCtrls,
  errorDialog, statistik_u, libraryAccess, sendBackError, Translations,
  progressDialog, bookListView, TreeListView, bookSearchForm, LCLType, lclproc,
  LCLIntf, process, applicationconfig;

const //automaticExtend=true;
      colorSelected=clHighlight;
      colorSelectedText=clHighlightText;
      colorStandardText=clBlack;
//const WM_ThreadException=wm_user+$321;

type
  { TmainForm }
  TmainForm = class(TForm)
    accountListMenuItem: TMenuItem;
    MenuItem10: TMenuItem;
    extendMenuList1: TMenuItem;
    extendTheseBooks: TMenuItem;
    MenuItem11: TMenuItem;
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
    MenuItem23: TMenuItem;
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
    TrayIconClick: TTimer;
    trayIconPopupMenu: TPopupMenu;
    removeSelectedMI: TMenuItem;
    displayDetailsMI: TMenuItem;
    searchDetailsMI: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem9: TMenuItem;
    bookPopupMenu: TPopupMenu;

    MenuItem8: TMenuItem;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
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
    procedure BookListDblClick(Sender: TObject);
    procedure bookPopupMenuPopup(Sender: TObject);
    procedure BookListUserSortItemsEvent(sender: TObject;
      var sortColumn: longint; var invertSorting: boolean);
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
    procedure FormWindowStateChange(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure MenuItem31Click(Sender: TObject);
    procedure removeSelectedMIClick(Sender: TObject);
    procedure displayDetailsMIClick(Sender: TObject);
    procedure repeatedCheckTimerTimer(Sender: TObject);
    procedure searchTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TrayIcon1Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure TrayIconClickTimer(Sender: TObject);
    procedure TreeListView1Click(Sender: TObject);
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
    lastTrayIconClick: dword;
  public
    { public declarations }
    oldListViewWindowProc: TWndMethod;

    searcherForm: TbookSearchFrm;

    BookList: TBookListView;


    procedure setPanelText(panel: TStatusPanel; atext: string);
    procedure ThreadDone(Sender: TObject);
    procedure RefreshListView; //Zentrale Anzeige Funktion!
    procedure refreshShellIntegration;
    procedure setSymbolAppearance(showStatus: integer); //0: text and icons 1=only text 2=only icons
    procedure refreshAccountGUIElements();
    function addAccount(const libID: string; prettyName, aname, pass: string; extendType: TExtendType; extendDays:integer; history: boolean):TCustomAccountAccess;
    //procedure WndProc(var TheMessage : TLMessage); override;
    function menuItem2AssociatedAccount(mi: TMenuItem): TCustomAccountAccess;


    procedure showVidelibri(var m:lcltype.TMsg); message LM_SHOW_VIDELIBRI;
    procedure showMainwindow;
  end;



var
  mainForm: TmainForm;
const SB_PANEL_COUNT=2;

procedure sendMailReports();

implementation

{ TmainForm }
uses math,options,newaccountwizard_u,registrierung,nagform,bbdebugtools,bibtexexport,booklistreader{$IFDEF WIN32},windows{$ENDIF},Clipbrd,bbutils;

function sendMailReportCompare(Item1, Item2: Pointer): Integer;
var book1,book2: TBook;
begin
  book1:=tbook(item1);
  book2:=tbook(item2);
  if book1.limitDate<book2.limitDate then exit(-1);
  if book1.limitDate>book2.limitDate then  exit(1);
  if (book1.status in BOOK_NOT_EXTENDABLE) and (book2.status in BOOK_EXTENDABLE) then exit(-1);
  if (book1.status in BOOK_EXTENDABLE) and (book2.status in BOOK_NOT_EXTENDABLE) then  exit(1);
  if book1.id < book2.id then exit(-1);
  if book1.id > book2.id then exit(1);
  exit(0);
end;

procedure sendMailReports();
function mailDate(d: integer):string;
begin
  result := DateToStr(d);
  if DateToPrettyStr(d) <> result then
    result += ' ('+DateToPrettyStr(d)+')';
end;

var prog: string;
 i: Integer;
 tmpbl: TBookList;
 usedaccounts: TFPList;
 accounts: TStringArray;
 count: LongInt;
 receiver: String;
 interval: LongInt;
 j: Integer;
 k: Integer;
 report: String;
 week: LongInt;
 proc: TProcess;
 subject: String;
 nextLimitDate, nextLimitDateNotExtendable: integer;

begin
  prog := userConfig.ReadString('Mail', 'Sendmail', 'sendmail -i -f "$from" $to');
  count := userConfig.ReadInteger('Mail', 'Reportcount', 0);
  tmpbl:=TBookList.Create;
  usedaccounts:=TFPList.Create;
  for i:=0 to count-1 do begin
    receiver :=userConfig.ReadString('Mailreport'+IntToStr(i), 'To', '');
    accounts :=strSplit(userConfig.ReadString('Mailreport'+IntToStr(i), 'Accounts',  ''),',');
    interval := userConfig.ReadInteger('Mailreport'+IntToStr(i), 'Interval', 1);
    if userConfig.ReadInteger('Mailreport'+IntToStr(i), 'Lastsent', 0) + interval > currentDate then continue;

    tmpbl.clear;
    for k:=0 to accountIDs.Count-1 do
      for j:= 0 to high(accounts) do
        if strContains(TCustomAccountAccess(accountIDs.Objects[k]).getID(), strTrim(accounts[j])) then begin
          tmpbl.addList(TCustomAccountAccess(accountIDs.Objects[k]).books.current);
          usedaccounts.add(accountIDs.Objects[k]);
        end;

    tmpbl.Sort(@sendMailReportCompare);

    subject:= 'Videlibri Bücherreport vom '+DateToStr(currentDate);
    report := 'Videlibri Bücherreport vom '+DateToStr(currentDate)+#13#10#13#10;
    if tmpbl.count = 0 then begin
      report += 'Keine Bücher registriert'#13#10;
    end else begin
      if (tmpbl.nextLimitDate(false) <= tmpbl.nextLimitDate()) or (tmpbl.nextLimitDate(false) < currentDate + 3) then
        subject := 'Nächste Frist: '  + mailDate(tmpbl.nextLimitDate()) + '(NICHT verlängerbar)' + ' | ' + subject
       else
        subject := 'Nächste Frist: '  + mailDate(tmpbl.nextLimitDate()) + ' | ' + subject;

      week := dateToWeek(tmpbl.books[j].limitDate);
      for j:=0 to tmpbl.Count-1 do begin
        if dateToWeek(tmpbl.books[j].limitDate) <> week then begin
          week := dateToWeek(tmpbl.books[j].limitDate);
          report += #13#10#13#10;
        end;
        if tmpbl.books[j].owner <> nil then report += TCustomAccountAccess(tmpbl.books[j].owner).prettyName + ': ';
        report +=  tmpbl.books[j].toSimpleString() + ':  ' + BookStatusToStr(tmpbl.books[j]) +  '  =>  ' +  mailDate(tmpbl.books[j].limitDate);
        report += #13#10;
      end;
    end;

    report += #13#10'Daten von: ';
    for j:=0 to usedaccounts.Count-1 do
      with TCustomAccountAccess(usedaccounts[j]) do begin
        report+=#13#10'        '+prettyName+': '+DateToPrettyStr(lastCheckDate);
        if not enabled then report+=' (DEAKTIVIERT!)';
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
    img,mask: graphics.TBITMAP;
    order: array[0..100] of longint;
begin
  if logging then log('FormCreate started');

  TranslateUnitResourceStrings('LCLStrConsts',dataPath+'lclstrconsts.de.po');

  setSymbolAppearance(userConfig.ReadInteger('appearance','symbols',0));

  
  tempItem:=TMenuItem.Create(libraryList);
  for i:=0 to libraryManager.getLibraryCountInEnumeration-1 do begin
    tempItem:=TMenuItem.Create(libraryList);
    tempItem.caption:=libraryManager.getLibraryFromEnumeration(i).prettyNameShort;
    tempItem.Tag:=i;
    tempItem.OnClick:=@LibraryHomepageClick;
    libraryList.Items.Add(tempItem);
  end;
  //TODO0: Passwort änderung
  //TODO1: Bessere Statistik
  tempItem:=libraryList.items[0];
  tempItem.caption:='alle';
  tempItem.Tag:=-1;
  tempItem.OnClick:=@LibraryHomepageClick;
  //libraryList.Items.Add(tempItem);

  refreshAccountGUIElements();

  BookList:=TBookListView.create(self,true);
  BookList.Parent:=self;
  BookList.BackGroundColor:=colorOK;
  BookList.Options:=BookList.Options-[tlvoStriped]+[tlvoMultiSelect,tlvoSorted];
  BookList.PopupMenu:=bookPopupMenu;
  BookList.OnSelect:=@BookListSelectItem;
  BookList.SortColumn:=BL_BOOK_COLUMNS_LIMIT_ID;
  BookList.OnUserSortItemsEvent:=@BookListUserSortItemsEvent;
  BookList.OnDblClick:=@BookListDblClick;
 // BookList.TabOrder:=0;

  BookList.deserializeColumnOrder(userConfig.ReadString('BookList','ColumnOrder',''));
  BookList.deserializeColumnWidths(userConfig.ReadString('BookList','ColumnWidths',''));
  BookList.deserializeColumnVisibility(userConfig.ReadString('BookList','ColumnVisibility',''));
  BookList.createUserColumnVisibilityPopupMenu();
  BookList.createSearchBar();
  BookList.SearchBar.Caption:='Ausleihensuche:';
  BookList.SearchBar.SearchForwardText:='&Vorwärts';
  BookList.SearchBar.SearchBackwardText:='&Rückwärts';
  BookList.SearchBar.SearchLocations[0]:='alle Felder';
  BookList.SearchBar.SearchLocations.InsertObject(0,'Verfasser/Titel',tobject(1 shl BL_BOOK_COLUMNS_AUTHOR or 1 shl BL_BOOK_COLUMNS_TITLE));

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

  {$ifdef debug}
  caption:=caption+' (DEBUG-BUILD!)';
  {$endif}

  RefreshShellIntegration;
    //image1.Picture.LoadFromFile(programPath+'images'+DirectorySeparator+IMAGE_FILES[0]+'.bmp');
  if logging then log('FormCreate ende');
end;

procedure TmainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TmainForm.FormResize(Sender: TObject);
var i:integer;
    item:TListItem;
    rec: Trect;
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
var rsc,i: longint;
begin
{  extendThisBooks.Enabled:=(ListView1.SelCount=1) and
                  (PBook(ListView1.Selected.Data))^.lib.getLibrary().canModifySingleBooks;}
  if BookList.selCount=0 then rsc:=0
  else begin
    rsc:=BookList.selcount;
    for i:=0 to BookList.Items.Count-1 do
      if BookList.Items[i].Selected and (BookList.Items[i].data.obj=nil) then rsc:=0;
  end;
  extendTheseBooks.Enabled:=(rsc>=1);
  extendAdjacentBooks.Enabled:=rsc=1;
  displayDetailsMI.Enabled:=rsc=1;
  searchDetailsMI.Enabled:=rsc=1;
  removeSelectedMI.Enabled:=rsc>0;
end;

procedure TmainForm.BookListDblClick(Sender: TObject);
begin
  if BookList.selCount=1 then displayDetailsMIClick(displayDetailsMI);
end;




procedure TmainForm.BookListUserSortItemsEvent(sender: TObject;
  var sortColumn: longint; var invertSorting: boolean);
begin
end;

procedure TmainForm.dailyCheckThreadTimer(Sender: TObject);
//this will be called every 24h to ensure that videlibri also works if you
//never turn your computer of
begin
  if alertAboutBooksThatMustBeReturned then show
  else begin
    accountsRefreshedToday:=false;
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
  extendBooks(MaxLongint,tbook(BookList.Selected.data.obj).owner as TCustomAccountAccess);
end;

procedure TmainForm.FormActivate(Sender: TObject);
begin
  if logging then log('FormActivate started');
  setPanelText(StatusBar1.Panels[3],{'Datum: '+}DateToStr(currentDate));
  if newVersionInstalled then
    ShowMessage('Das Update wurde installiert.'#13#10'Die installierte Version ist nun Videlibri '+FloatToStr(versionNumber/1000));
  onshow:=nil;
  windowstate:=twindowstate(userConfig.ReadInteger('window','state',integer(windowstate)));
  if accountIDs.count>0 then begin
    RefreshListView;
    if not needApplicationRestart then
      defaultAccountsRefresh;
  end;
  if not needApplicationRestart then
    if accountIDs.count=0 then begin
      repeat
        with TnewAccountWizard.Create(nil) do begin
          ShowModal;
          free;
        end;
      until (accountIDs.count<>0) or (Application.MessageBox('Sie müssen ein Konto angeben, um VideLibri benutzen zu können.'#13#10'Wollen Sie eines eingeben?','Videlibri',MB_YESNO or MB_ICONQUESTION)=mrNo);
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

  userConfig.WriteString('BookList','ColumnOrder',BookList.serializeColumnOrder);
  userConfig.WriteString('BookList','ColumnWidths',BookList.serializeColumnWidths);
  userConfig.WriteString('BookList','ColumnVisibility',BookList.serializeColumnVisibility);

  if searcherForm <> nil then searcherForm.saveDefaults;
  FreeAndNil(searcherForm);
  FreeAndNil(BookList);
end;

procedure TmainForm.BookListSelectItem(Sender: TObject; Item: TTreeListItem);
var s:string;
begin
  s:=StatusBar1.Panels[SB_PANEL_COUNT].Text;
  if strscan(pchar(s),'/')<>strrscan(pchar(s),'/') then begin
    //Format: Medien: 3/4/5
    delete(s,1,pos('/',s));
  end else  //Format: Medien: 4/5
    delete(s,1,pos(' ',s));
  if BookList.SelCount=0 then setPanelText(StatusBar1.Panels[SB_PANEL_COUNT],'Medien: '+s)
  else setPanelText(StatusBar1.Panels[SB_PANEL_COUNT],'Medien: '+IntToStr(BookList.SelCount)+'/'+s);
end;

procedure TmainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState=wsMinimized then hide
  else lastState := WindowState;
end;

procedure TmainForm.MenuItem11Click(Sender: TObject);
var i:integer;
    books:TBookList;
begin
  books:=TBookList.Create;
  books.Capacity:=BookList.selCount;
  for i:=0 to BookList.Items.Count-1 do
    if BookList.Items[i].Selected and (BookList.Items[i].data.obj<>nil) then
      books.add(TBook(BookList.Items[i].data.obj));
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
  filename:='"'+dataPath+ filename+'"';
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
  showCHM('videlibri.chm',1000, 'Fensterbeschreibung');
end;

procedure TmainForm.MenuItem15Click(Sender: TObject);
begin
  showCHM('videlibri.chm',1001, 'Fragen und Antworten');
end;

procedure TmainForm.MenuItem16Click(Sender: TObject);
begin
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
  'Verwendete Fremdkomponenten:'#13#10'  LCL'#13#10'  FreePascal Runtime'#13#10'  TRegExpr von Andrey V. Sorokin'#13#10#13#10+
  'Verwendete Entwicklungswerkzeuge:'#13#10'  Lazarus'#13#10'  FreePascal'#13#10'  GIMP'#13#10'  HTML Help Workshop'#13#10'  The Regex Coach'#13#10#13#10+
  'Danke an: Leonid Sokolov, Martin Kim Dung-Pham'#13#10#13#10+
  'Die angezeigten Daten stammen von/gehören den jeweiligen Bibliotheken, dem HBZ und Amazon. '+{#13#10'  den Stadtbüchereien Düsseldorf, Aachen'#13#10+
  '  der Universitäts- und Landesbibliothek Düsseldorf'#13#10+
  '  den Fachhochschulbüchereien Düsseldorfs und Bochums'#13#10+
  '  der DigiBib/dem Hochschulbibliothekszentrum des Landes Nordrhein-Westfalen'#13#10+
  '  der Universitätsbibliothek der TU und HU Berlin'#13#10+
  '  Amazon.com';                                           }'';
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
  applicationUpdate(false);
end;

procedure TmainForm.MenuItem23Click(Sender: TObject);
var regForm:TRegForm;
begin
  regForm:=TRegForm.Create(nil);
  regForm.ShowModal;
  regForm.free;
  if needApplicationRestart then close;
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
  if searcherForm=nil then searcherForm:=TbookSearchFrm.Create(nil);
  searcherForm.loadDefaults;
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
begin
  for i:=0 to BookList.Items.count-1 do
    if ((BookList.selCount = 0) or (BookList.Items[i].Selected)) and ((BookList.Items[i].data.obj <> nil) and (BookList.books[i].owner<>nil)) then begin
      t += (BookList.books[i].owner as TCustomAccountAccess).prettyName + ':  ' + BookList.books[i].toLimitString() + LineEnding;
    end;
  clipboard.astext := t;
end;

procedure TmainForm.removeSelectedMIClick(Sender: TObject);
var i:longint;
    accountsToSave: tlist;
    book: TBook;
begin
  if updateThreadConfig.updateThreadsRunning>0 then begin
    ShowMessage('Diese Aktion kann nicht durchgeführt werden, solange Medien aktualisiert werden.') ;
    exit;
  end;
  for i:=0 to BookList.Items.count-1 do
    if (BookList.Items[i].Selected) then
      if BookList.Items[i].data.obj=nil then begin
        ShowMessage('Es ist momentan ein Eintrag markiert, bei dem es sich nicht um ein Buch handelt'#13#10'Bitte demarkieren Sie ihn.');
        exit;
      end else if tbook(BookList.Items[i].data.obj).lend then begin
        ShowMessage('Es sind momentan ausgeliehene Medien markiert, es können aber nur abgegebene Medien aus der History gelöscht werden'#13#10'Bitte demarkieren Sie sie.');
        exit;
      end;
  accountsToSave:=tlist.Create;
  try
    for i:=0 to BookList.Items.count-1 do
      if (BookList.Items[i].Selected) then begin
        book:=tbook(BookList.Items[i].data.obj);
        if accountsToSave.IndexOf(book.owner)<0 then accountsToSave.Add(book.owner);
        if TCustomAccountAccess(book.owner).books.old.Remove(book) < 0 then begin
          ShowMessage('Das markierte Buch war nicht vorhanden.'#13#10'Das ist eigentlich unmöglich, am besten starten Sie das Programm neu.');
          exit;
        end;
      end;
    for i:=0 to accountsToSave.Count-1 do
      TCustomAccountAccess(accountsToSave[i]).save();
  RefreshListView;
  finally
    accountsToSave.Free;
  end;
end;

procedure TmainForm.displayDetailsMIClick(Sender: TObject);
begin
  if (BookList.Selected = nil) or (BookList.Selected.data.obj=nil) then exit;
  if searcherForm=nil then searcherForm:=TbookSearchFrm.Create(nil);
  searcherForm.selectBookToReSearch(tbook(BookList.Selected.data.obj));
  if TComponent(sender).tag<>1 then searcherForm.startSearch.Click;
  searcherForm.Show;
end;

procedure TmainForm.repeatedCheckTimerTimer(Sender: TObject);
//this tries to update the books every 5min until a connection to the library
//could actually established
var internet: TInternetAccess;
begin
  if accountsRefreshedToday then begin
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
  if GetTickCount - lastTrayIconClick < 250 then exit;
  lastTrayIconClick:=GetTickCount;
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
  BookList.SearchBar.Show;
  BookList.SearchBar.SetFocus;
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
    id,title,extraParams:string;
    lib:TLibrary;
begin
  if tcontrol(sender).Tag=-1 then begin
    baseURL:='http://www.digibib.net/Digibib?SERVICE=SESSION&SUBSERVICE=GUESTLOGIN&LOCATION=DUEBIB&LANGUAGE=de';
    id:='xxx';
    title:='DigiBib: Düsseldorfer Bibliotheken';
    extraParams:='';
  end else begin
    lib:=libraryManager.getLibraryFromEnumeration(tcontrol(sender).Tag);
    baseURL:=lib.homepage;
    id:=lib.id;
    title:=TMenuItem(sender).Caption;
    extraParams:='';
    //if not lib.allowHomepageNavigation then extraParams:=extraParams+' /no-navigation';
    if lib.bestHomepageWidth>0 then extraParams:=extraParams+' /pagewidth='+InttoStr(lib.bestHomepageWidth);
    if lib.bestHomepageHeight>0 then extraParams:=extraParams+' /pageheight='+InttoStr(lib.bestHomepageHeight);
  end;
//  ShowMessage(TMenuItem(sender).Caption);
  openInternetPage(baseURL,'/title="'+title+'" /configfile="'+userPath+'browserconfig.ini" /windowsection='+id+' '+extraParams);
  //ShowMessage(programPath+'simpleBrowser /title="'+TMenuItem(sender).Caption+'" /site="'+baseURL+'" /configfile="'+userPath+'browerconfig.ini" /windowsection='+id);
end;

procedure TmainForm.StatusBar1DblClick(Sender: TObject);
var pos: TPoint;
    i,j:integer;
    s:string;
begin
  GetCursorPos(pos);
  pos:=StatusBar1.ScreenToClient(pos);
  for i:=0 to StatusBar1. Panels.Count-1 do
    if pos.x<=statusbar1.panels.items[i].Width then begin
      case i of
        0: begin
             s:='Die angezeigten Daten der Konten wurden zu folgenden Zeitpunkten'#13#10'das letztemal aktualisiert:';
             for j:=0 to accountIDs.count-1 do
               with TCustomAccountAccess(accountIDs.Objects[j]) do begin
                 s:=s+#13#10'        '+prettyName+': '+DateToPrettyStr(lastCheckDate);
                 if not enabled then s+=' (DEAKTIVIERT!)';
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

procedure TmainForm.RefreshListView;
var i,j,count,count2:integer;
    book:TBook;
    books:TBookLists;
    typ: TBookOutputType;
    criticalSessionUsed,oldList,currentList:boolean;
    account: TCustomAccountAccess;
    maxcharges:currency;
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
  BookList.BeginUpdate;
  BookList.items.clear;
  for i:=0 to viewMenu.Count-1 do begin
    if (viewMenu.Items[i].tag = -1) then continue;
    if not viewMenu.Items[i].Checked then continue;
    assert(viewMenu.Items[i].tag>=1);
    assert(viewMenu.Items[i].tag<=accountIDs.Count);
    account:=TCustomAccountAccess(accountIDs.Objects[viewMenu.Items[i].tag-1]);
    if currentList then begin
      bookList.addBookList(account.books.current);
      lastCheck:=min(lastCheck,account.books.current.lastCheck);
    end;
    if oldList then bookList.addBookList(account.books.old);
  end;
  BookList.Sort;

  maxcharges:=0;
  for i:=0 to accountIDs.Count-1 do
    maxcharges:=maxcharges+TCustomAccountAccess(accountIDs.Objects[i]).charges;

  if maxcharges>0 then begin
    StatusBar1.Panels[1].text:='Offene Gebühren: '+floattostr(maxcharges)+'€ (';
    for i:=0 to accountIDs.Count-1 do
        if TCustomAccountAccess(accountIDs[i]).charges>0 then
           StatusBar1.Panels[1].text:=StatusBar1.Panels[1].text+
              TCustomAccountAccess(accountIDs[i]).prettyName+': '+
              FloatToStr(TCustomAccountAccess(accountIDs[i]).charges) +'€ ';
    setPanelText(StatusBar1.Panels[1],StatusBar1.Panels[1].text+')');
  end else setPanelText(StatusBar1.Panels[1],'');
  setPanelText(StatusBar1.Panels[SB_PANEL_COUNT],'Medien: '+IntToStr(BookList.Items.Count));


  if updateThreadConfig.updateThreadsRunning<=0 then begin
    setPanelText(StatusBar1.Panels[0],'Älteste angezeigte Daten sind '+DateToPrettyGrammarStr('vom ','von ',lastCheck));

  end;

  RefreshShellIntegration();

  BookList.EndUpdate;


  for i:=0 to viewMenu.Count-1 do begin
    if (viewMenu.Items[i].tag = 0) or (viewMenu.Items[i].tag = -1) then continue;
    if menuItem2AssociatedAccount(viewMenu.Items[i]).enabled then continue;


    BookList.BeginUpdate;
    with BookList.items.Add do begin
      text:='ACHTUNG';
      RecordItems.Add('');
      RecordItems.Add('VideLibri');
      RecordItems.Add('dieses Konto ist deaktiviert und der Ausleihstatus ist folgedessen unbekannt');
      RecordItems.Add('');
      RecordItems.Add('');
      RecordItems.Add('');
      RecordItems.Add(menuItem2AssociatedAccount(viewMenu.Items[i]).prettyName);
      RecordItems.Add('');
      RecordItems.Add('');
      RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR]:='clRed';
      //SubItems.add('');
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
begin
  MenuItem29.Caption:='  Nächste Abgabefrist: '+nextLimitStr;
  TrayIcon1.Hint:='Videlibri'#13#10'  **Nächste Abgabefrist: '+nextLimitStr+'**'#13#10'  Letzte Aktualisierung: '+DateToPrettyStr(lastCheck);
  TrayIcon1.Icon.LoadFromFile(getTNAIconFileName());
  icon.LoadFromFile(getTNAIconFileName());
  Application.icon.LoadFromFile(getTNAIconFileName());
end;


procedure TmainForm.setSymbolAppearance(showStatus: integer);
var bheight: integer;
begin
  bheight:=0;
  if showStatus and 2=0 then bheight:=bHeight+17;
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
    newItem.Caption:=TCustomAccountAccess(accountIDs.Objects[id]).prettyName;
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
    account: TCustomAccountAccess;
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
  for i:=0 to accountIDs.Count-1 do begin
    account:=TCustomAccountAccess(accountIDs.Objects[i]);
    accountListMenu.Items.Add(newItem(accountListMenu,@refreshAccountClick,i));
    accountListMenuItem.Add(newItem(accountListMenu,@refreshAccountClick,i));
    temp:=newItem(accountListMenu,@showAccount,i);
    temp.Checked:=true;
    viewMenu.Add(temp);
    addToMenuItem(extendMenuList1,@UserExtendMenuClick,i);
    for j:=0 to extendMenuList2_.Count-1 do
      addToMenuItem(extendMenuList2_[j],@UserExtendMenuClick,i);
  end;
end;

function TmainForm.addAccount(const libID: string; prettyName, aname, pass: string; extendType: TExtendType; extendDays:integer; history:boolean):TCustomAccountAccess;
begin
{if accountList.Selected <> nil then
    if (accountList.Selected.Caption=edtAccountPrettyName.Text) or
       (accountList.Selected.SubItems[0]=edtAccountUser.Text) then begin
      ShowMessage('Das Konto existiert bereits auf diesem Computer und kann deshalb nicht erstellt werden.   '#13#10+
                  'Falls Sie eine Eigenschaft von einem Konto ändern wollen, klicken Sie bitte auf den Button "Konto ändern"'#13#10+
                  'Falls Sie das Konto neu erstellen wollen, löschen Sie bitte das zuerst das alte, und erstellen es dann neu');
      exit;
   end;   }

  result:=libraryManager.getAccount(libID,aname);
  result.prettyName:=prettyName;
  result.password:=pass;
  result.keepHistory:=history;//ckbAccountHistory.Checked;
  result.extendType:=extendType;// TExtendType( cmbAccountExtend.ItemIndex));
  result.extendDays:=extendDays;// StrToInt(edtAccountExtendDays.Text));

  if optionForm<>nil then
    optionForm.addAccount(result);


  accountIDs.AddObject(result.getID(),result);
  result.save();
  saveLibIDs;

  refreshAccountGUIElements();
{  if MessageDlg('Daten laden?',
                'Das Konto '+lib.getPrettyName()+' wurde erstellt.'#13#10'Sollen jetzt die Mediendaten heruntergeladen werden?',
                mtConfirmation ,[mbYes,mbNo],0)=mrYes then
    mainForm.updateLibrary(lib,false,false);}
end;

function TmainForm.menuItem2AssociatedAccount(mi: TMenuItem
  ): TCustomAccountAccess;
begin
  if (mi.tag<=0) or (mi.tag>accountIDs.Count) then
    raise exception.Create('Ungültiges Konto angegeben');
  result:=TCustomAccountAccess(accountIDs.Objects[mi.tag-1]);
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
  libraryAccess.threadDone(Sender); //self=nil !!!
end;


initialization
  {$I bookwatchmain.lrs}
end.