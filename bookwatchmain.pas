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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, libraryParser,internetAccess, ComCtrls, Menus,
  lmessages, ExtCtrls,errorDialog,statistik_u,libraryAccess,sendBackError,Translations,progressDialog,
  bookListView,TreeListView, bookSearchForm, LCLType, lclproc, LCLIntf;

const //automaticExtend=true;
      colorSelected=clHighlight;
      colorSelectedText=clHighlightText;
      colorStandardText=clBlack;
//const WM_ThreadException=wm_user+$321;

type
  { TmainForm }
  TmainForm = class(TForm)
    accountListMenuItem: TMenuItem;
    Label2: TLabel;
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
    procedure delayedCallTimer(Sender: TObject);
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
    procedure removeSelectedMIClick(Sender: TObject);
    procedure displayDetailsMIClick(Sender: TObject);
    procedure searchTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TrayIcon1Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
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

  public
    { public declarations }
    oldListViewWindowProc: TWndMethod;

    searcherForm: TbookSearchFrm;

    BookList: TBookListView;


    procedure ThreadDone(Sender: TObject);
    procedure RefreshListView; //Zentrale Anzeige Funktion!
    procedure setSymbolAppearance(showStatus: integer); //0: text and icons 1=only text 2=only icons
    procedure refreshAccountGUIElements();
    procedure updateGUIItemsForAccount(const account: TCustomAccountAccess);
    procedure removeGUIItemsForAccount(const account: TCustomAccountAccess);
    function addAccount(const libID: string; prettyName, aname, pass: string; extendType: TExtendType; extendDays:integer; history: boolean):TCustomAccountAccess;
    //procedure WndProc(var TheMessage : TLMessage); override;
    function menuItem2AssociatedAccount(mi: TMenuItem): TCustomAccountAccess;

  end;



var
  mainForm: TmainForm;
const SB_PANEL_COUNT=2;


implementation

{ TmainForm }
uses math,options,applicationconfig,newaccountwizard_u,registrierung,nagform,bbdebugtools,bibtexexport,booklistreader;

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
  
 { img:=graphics.TBitmap.Create;
  mask:=graphics.TBitmap.Create;
  ImageList1.Clear;
  for i:=0 to high(IMAGE_FILES) do begin
    img.LoadFromFile(programPath+'images'+DirectorySeparator+IMAGE_FILES[i]+'.bmp');
 //   mask.LoadFromFile(programPath+'images'+DirectorySeparator+IMAGE_FILES[i]+'_mask.bmp');
    ImageList1.Add(img,nil)
  end;       }

  caption:=appFullTitle;


  TrayIcon1.Icon.LoadFromFile(getTNAIconFileName());

  {$ifdef debug}
  caption:=caption+' (DEBUG-BUILD!)';
  {$endif}


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
  if pos('shareware',lowercase(Label2.Caption))>0 then RefreshListView;
end;

procedure TmainForm.delayedCallTimer(Sender: TObject);
begin
  delayedCall.Enabled:=false;
 showErrorMessages();
end;

procedure TmainForm.extendAdjacentBooksClick(Sender: TObject);
begin
  if (BookList.Selected = nil) or (BookList.Selected.data.obj=nil) then exit;
  extendBooks(MaxLongint,tbook(BookList.Selected.data.obj).owner as TCustomAccountAccess);
end;

procedure TmainForm.FormActivate(Sender: TObject);
var ok3,nok1,nok3:longbool;
{$include _shareware.inc}
var nag:TnagWindow;
    sharewaretest,ok1,ok2,nok2:longbool;
    user,code:string;
begin
  if logging then log('FormActivate started');
  //SHAREWARE CODE
  {$I obfuscate.inc}
  user:=sharewareUser;
  {$I obfuscate.inc}
  code:=sharewareCode;
  {$I obfuscate.inc}
  sharewaretest:=checkShareWare(ok1,nok1,user,code,ok2,nok2);
  OnActivate:=nil;
  {$I obfuscate.inc}
  nag:=TnagWindow.create(nil);
  //SHAREWARE CODE END
  StatusBar1.Panels[3].Text:={'Datum: '+}DateToStr(currentDate);
  if newVersionInstalled then
    ShowMessage('Das Update wurde installiert.'#13#10'Die installierte Version ist nun Videlibri '+FloatToStr(versionNumber/1000));
  onshow:=nil;
  windowstate:=twindowstate(userConfig.ReadInteger('window','state',integer(windowstate)));
  //SHAREWARE CODE
  {$I obfuscate.inc}
  if (not sharewaretest) then nag.ShowModal
  else Label2.Caption:='';
  {$I obfuscate.inc}
  if sharewaretest <> ok2 then halt;
  if (ok1=nok1) then halt;
  if sharewaretest <> ok1 then halt;
  {$I obfuscate.inc}
  if 6785<>nag.tag then begin
    {$I obfuscate.inc}
    if currentDate>39264 then if nok2 or nok1 then halt;
  end;
  //SHAREWARE CODE END
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
      until (accountIDs.count<>0) or (MessageBoxUTF8('Sie müssen ein Konto angeben, um VideLibri benutzen zu können.'#13#10'Wollen Sie eines eingeben?',MB_YESNO or MB_ICONQUESTION)=mrNo);
    end;
  //SHAREWARE CODE
  nag.Free;
  //SHAREWARE CODE END
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
  if BookList.SelCount=0 then StatusBar1.Panels[SB_PANEL_COUNT].Text:='Medien: '+s
  else StatusBar1.Panels[SB_PANEL_COUNT].Text:='Medien: '+IntToStr(BookList.SelCount)+'/'+s;
  if StatusBar1.Canvas.TextWidth(StatusBar1.Panels[SB_PANEL_COUNT].Text)+10>StatusBar1.Panels[SB_PANEL_COUNT].Width then
    StatusBar1.Panels[SB_PANEL_COUNT].Width:=StatusBar1.Canvas.TextWidth(StatusBar1.Panels[SB_PANEL_COUNT].Text)+10;
end;

procedure TmainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState=wsMinimized then hide;
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

procedure TmainForm.MenuItem14Click(Sender: TObject);
begin
  //TODO: WinExec('hh -mapid 1000 videlibri.chm',sw_shownormal);
end;

procedure TmainForm.MenuItem15Click(Sender: TObject);
begin
  //TODO: WinExec('hh -mapid 1001 videlibri.chm',sw_shownormal);
end;

procedure TmainForm.MenuItem16Click(Sender: TObject);
begin
  TsendBackErrorForm.openErrorWindow(oldErrorMessageString,IntToStr(versionNumber),'Videlibri');
end;

procedure TmainForm.MenuItem17Click(Sender: TObject);
  type tpopenAboutDialog = procedure (title,description,creditlist: pchar; modal: boolean; game: longint);stdcall;
var s1, s2, s3: string;
    openAboutDialog: tpopenAboutDialog;
    lib:thandle;
begin
  //TODO:
  (*lib:=LoadLibrary('bbabout.dll');
  openAboutDialog:=tpopenAboutDialog(GetProcAddress(lib,'openAboutDialog'));
  if assigned(openAboutDialog) then
    openAboutDialog(pchar({'Videlibri '+FloatToStr(versionNumber/1000)}caption),'',pchar(Utf8ToAnsi(
      'Verwendete Fremdkomponenten:'#13#10'  LCL'#13#10'  FreePascal Runtime'#13#10'  TRegExpr von Andrey V. Sorokin'#13#10#13#10+
      'Verwendete Entwicklungswerkzeuge:'#13#10'  Lazarus'#13#10'  FreePascal'#13#10'  GIMP'#13#10'  HTML Help Workshop'#13#10'  The Regex Coach'#13#10#13#10+
      'Die angezeigten Daten stammen von/gehören: '#13#10'  den Stadtbüchereien Düsseldorf'#13#10+
      '  der Universitäts- und Landesbibliothek Düsseldorf'#13#10+
      '  den Fachhochschulbüchereien Düsseldorfs und Bochums'#13#10+
      '  der DigiBib/dem Hochschulbibliothekszentrum des Landes Nordrhein-Westfalen'#13#10+
      '  Amazon.com')),true,1);
  FreeLibrary(lib);              *)
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
  Application.CreateForm(TBibTexExportFrm,bibexportForm);
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
  searcherForm.ShowModal;
  searcherForm.saveDefaults;

end;

procedure TmainForm.MenuItem26Click(Sender: TObject);
begin
  TrayIcon1DblClick(self);
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
  searcherForm.ShowModal;
end;

procedure TmainForm.searchTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TmainForm.TrayIcon1Click(Sender: TObject);
begin

end;

procedure TmainForm.TrayIcon1DblClick(Sender: TObject);
//TODO:  function ForceForegroundWindow(hwnd: THandle): WordBool;
{const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);

  if GetForegroundWindow = hwnd then Result := True
  else
  begin
    // Windows 98/2000 doesn't want to foreground a window when some other
    // window has keyboard focus

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
      ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
      ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
      (Win32MinorVersion > 0)))) then
    begin
      // Code from Karl E. Peterson, www.mvps.org/vb/sample.htm
      // Converted to Delphi by Ray Lischner
      // Published in The Delphi Magazine 55, page 16

      Result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadPRocessId(hwnd, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
        Result := (GetForegroundWindow = hwnd);
      end;
      if not Result then
      begin
        // Code by Daniel P. Stasinski
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
          SPIF_SENDCHANGE);
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hWnd);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
      end;
    end
    else
    begin
      BringWindowToTop(hwnd); // IE 5.5 related hack
      SetForegroundWindow(hwnd);
    end;

    Result := (GetForegroundWindow = hwnd);
  end;
end;} { ForceForegroundWindow }
begin
  //TODO:mainform.WindowState:=wsNormal;?
  mainform.show;
  mainform.BringToFront;
  //TODO: ForceForegroundWindow(mainform.Handle);
end;


procedure TmainForm.UserExtendMenuClick(Sender: TObject);
var limitTime: longint;
begin
  limitTime:=(sender as tmenuItem).Parent.tag;
  //TODO: extendBooks(limitTime+currentDate,TCustomAccountAccess(tmenuitem(sender).tag));
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
  Application.CreateForm(ToptionForm,optionsForm);
  optionsForm.ShowModal;
  optionsForm.free;
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

procedure TmainForm.RefreshListView;
var i,j,count,count2:integer;
    book:TBook;
    sharewaretest:boolean; //für Sharewaretest
    books:TBookLists;
    typ: TBookOutputType;
    criticalSessionUsed,oldList,currentList:boolean;
    account: TCustomAccountAccess;
    maxcharges:currency;
var ok1,ok2,ok3,nok1,nok2,nok3: longbool; //für Sharewaretest
    user,code:string;                                      //für Sharewaretest
    disabledAccoutsExists: Boolean;
label showOnly10;                  //für Sharewaretest
{$include _shareware.inc}
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

  //SHAREWARE CODE
  {$I obfuscate.inc}
  user:=sharewareUser;
  {$I obfuscate.inc}
  code:=sharewareCode;
  {$I obfuscate.inc}
  sharewaretest:=checkShareWare(ok1,nok1,user,code,ok2,nok2);
  {$I obfuscate.inc}
  if not sharewaretest then begin
    showOnly10:
    BookList.sort;
    {$I obfuscate.inc}
    for i:= BookList.items.count-1 downto 10 do begin;
      {$I obfuscate.inc}
      BookList.items.Delete(i);
    end;
  end;

  if sharewaretest<>ok1 then halt;

  j:=8972;
  {$I obfuscate.inc}
  while sharewaretest= nok3 do begin
    for i:=142 to 2302 do
      j:=(j+j*i+301) mod 32201;
    if j<-1 then break;
  end;

  if nok2=sharewaretest then close;
  //SHAREWARE CODE END


  maxcharges:=0;
  for i:=0 to viewMenu.Count-1 do begin
    if (viewMenu.Items[i].tag = 0) or (viewMenu.Items[i].tag = -1) then continue;
//    if TCustomAccountAccess(viewMenu.Items[i].Tag).charges>0 then
 //TODO:     maxcharges:=maxcharges+TCustomAccountAccess(viewMenu.Items[i].Tag).charges;
  end;

  //SHAREWARE CODE
  while ok2<>ok1 do maxcharges:=maxcharges+2;
  if nok1 = ok1 then close;
  //SHAREWARE CODE END

  if maxcharges>0 then begin
    StatusBar1.Panels[1].text:='Offene Gebühren: '+floattostr(maxcharges)+'€ (';
    for i:=0 to viewMenu.Count-1 do begin
      if (viewMenu.Items[i].tag = 0) or (viewMenu.Items[i].tag = -1) then continue;
      //TODO:if TCustomAccountAccess(viewMenu.Items[i].Tag).charges>0 then
        {StatusBar1.Panels[1].text:=StatusBar1.Panels[1].text+
          TCustomAccountAccess(viewMenu.Items[i].Tag).prettyName+': '+
          FloatToStr((TCustomAccountAccess(viewMenu.Items[i].Tag).charges)) +'€ ';}
    end;
    StatusBar1.Panels[1].text:=StatusBar1.Panels[1].text+')';
  end else StatusBar1.Panels[1].text:='';
  StatusBar1.Panels[SB_PANEL_COUNT].Text:='Medien: '+IntToStr(BookList.Items.Count);

  //SHAREWARE CODE
  {$I obfuscate.inc}
  if BookList.Items.count>10 then
    if nok2 then halt;
  //SHAREWARE CODE END

  if updateThreadConfig.updateThreadsRunning<=0 then begin
    StatusBar1.Panels[0].text:='Älteste angezeigte Daten sind '+DateToPrettyGrammarStr('vom ','von ',lastCheck);

  end;
  icon.LoadFromFile(getTNAIconFileName());
  Application.icon.LoadFromFile(getTNAIconFileName());
  //SHAREWARE CODE
  //Bücherzahl überprüfen

  BookList.EndUpdate;

  count:=0;
  count2:=0;
  for i:=0 to viewMenu.Count-1 do begin
    if (viewMenu.Items[i].tag = 0) or (viewMenu.Items[i].tag = -1) then continue;
    if not viewMenu.Items[i].Checked then continue;
    books:=TCustomAccountAccess(accountIDs.objects[viewMenu.Items[i].Tag-1]).books;
           //TCustomAccountAccess(viewMenu.Items[i].Tag).books;
    if currentList then count+=books.current.Count;
    if oldList then count+=books.old.Count;
    if viewMenu.Items[i].Checked then begin
      if currentList then count2+=books.current.Count;
      if oldList then count2+=books.old.Count;
    end;
  end;
  StatusBar1.Panels[SB_PANEL_COUNT].Text:=StatusBar1.Panels[SB_PANEL_COUNT].Text+'/'+inttostr(count);
  if count2>BookList.Items.count then begin
    BookList.BeginUpdate;
    with BookList.items.Add do begin
      text:='ACHTUNG';
      RecordItems.Add('');
      RecordItems.Add('VideLibri');
      RecordItems.Add('weitere '+IntToStr(count-BookList.items.count+1)+' Medien ausgeblendet');
      RecordItems.Add('');
      RecordItems.Add('');
      RecordItems.Add('');
      RecordItems.Add('');
      RecordItems.Add('Nur die Vollversion zeigt mehr als 10 Medien an.');
      RecordItems.Add('');
      RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR]:='clRed';
      //SubItems.add('');
      Tag:=0;
    end;
    BookList.EndUpdate;
  end else if count2>10 then
    if  (nok1 or not ok3) then close;
  //SHAREWARE CODE ENDE


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

  if criticalSessionUsed then
    system.LeaveCriticalSection(updateThreadConfig.libraryAccessSection);

  if logging then log('RefreshListView ended');
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
    if accountListMenu.items[i].tag>=1 then begin
      accountListMenu.items.Delete(i);
      break;
    end;

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

procedure TmainForm.updateGUIItemsForAccount(const account: TCustomAccountAccess);
  procedure checkMenuItem(item:TMenuItem);
  var i:integer;
  begin
    for i:=1 to item.Count-1 do
      if item.items[i].tag=longint(account) then begin
        item.items[i].caption:=account.prettyName;
        break;
      end;
  end;

var i:integer;
begin
  if account=nil then exit;
  checkMenuItem(accountListMenuItem);
  checkMenuItem(viewMenu);
  checkMenuItem(extendMenuList1);
  for i:=0 to extendMenuList2_.count-1 do
    checkMenuItem(extendMenuList2_[i]);
  for i:=0 to accountListMenu.items.count-1 do
    if accountListMenu.items[i].tag=longint(account) then begin
      accountListMenu.items[i].caption:=account.prettyName;
      break;
    end;
end;

procedure TmainForm.removeGUIItemsForAccount(const account: TCustomAccountAccess);
begin
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

  refreshAccountGUIElements();

  accountIDs.AddObject(result.getID(),result);
  result.save();
  saveLibIDs;

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

procedure TmainForm.ThreadDone(Sender: TObject);
//called in the main thread
begin
  libraryAccess.threadDone(Sender); //self=nil !!!

end;


initialization
  {$I bookwatchmain.lrs}
end.
