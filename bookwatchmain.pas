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
  TreeListView;

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
    MenuItem3: TMenuItem;
    MenuItem9: TMenuItem;
    bookPopupMenu: TPopupMenu;
    searchField: TComboBox;
    MenuItem8: TMenuItem;
    searchClose: TButton;
    searchText: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    searchStatus: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    searchPanel: TPanel;
    showAllAccounts: TMenuItem;
    showNoAccounts: TMenuItem;
    searchDown: TSpeedButton;
    searchUp: TSpeedButton;
    Timer1: TTimer;
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
    procedure appMinimize(Sender: TObject);
    procedure appRestore(Sender: TObject);
    procedure BookListCompareItems(sender: TObject; i1, i2: TTreeListItem;
      var compare: longint);
    procedure BookListCustomItemDraw(sender: TObject;
      eventTyp_cdet: TCustomDrawEventTyp; item: TTreeListItem; xpos, ypos,
      xColumn: integer; lastItem: boolean; var defaultDraw: Boolean);
    procedure BookListUserSortItemsEvent(sender: TObject;
      var sortColumn: longint; var invertSorting: boolean);
    procedure bookPopupMenuPopup(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure delayedCallTimer(Sender: TObject);
    procedure delayedPopupTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListView1CustomDraw(Sender: TCustomListView; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListView1CustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure ListView1MouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BookListSelectItem(Sender: TObject; Item: TTreeListItem);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure UserExtendMenuClick(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure Panel1MouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure searchCloseClick(Sender: TObject);
    procedure searchDownClick(Sender: TObject);
    procedure searchTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure searchUpClick(Sender: TObject);
    procedure showAccount(Sender: TObject);
    procedure refreshAccountClick(Sender: TObject);
    procedure LibraryHomepageClick(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
    procedure ToolBar1Resize(Sender: TObject);
    procedure ShowOptionsClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure ToolButton2ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ToolButton3Click(Sender: TObject);
    procedure ViewAllClick(Sender: TObject);

    procedure ListView1WindowProc(var TheMessage: TLMessage);
  private
    { private declarations }

  public
    { public declarations }
    oldListViewWindowProc: TWndMethod;
    searchMarkRow,searchMarkCol,searchMarkStart,searchMarkLen: longint;
    searchMarkVisible:boolean;

    BookList: TTreeListView;

    procedure searchInListView(step: integer=1;researchCurrentLine: boolean=false);
    procedure ThreadDone(Sender: TObject);
    procedure RefreshListView; //Zentrale Anzeige Funktion!
    procedure setSymbolAppearance(showStatus: integer); //0: text and icons 1=only text 2=only icons
    procedure addGUIItemsForNewAccount(const account: TCustomAccountAccess);
    procedure updateGUIItemsForAccount(const account: TCustomAccountAccess);
    procedure removeGUIItemsForAccount(const account: TCustomAccountAccess);
    function addAccount(const libID: string; prettyName, aname, pass: string; extendType: TExtendType; extendDays:integer; history: boolean):TCustomAccountAccess;
    procedure lm_rbuttonup(var mes: TLMRButtonUp); message lm_rbuttonup;
    //procedure WndProc(var TheMessage : TLMessage); override;
  end;



var
  mainForm: TmainForm;
const LV_BOOK_COLUMNS_YEAR_ID=8;
      SB_PANEL_COUNT=2;
      
const LVM_FIRST=$1000;
      LVM_SETCOLUMNORDERARRAY=LVM_FIRST + 58;
      LVM_GETCOLUMNORDERARRAY=LVM_FIRST + 59;


implementation

{ TmainForm }
uses windows,math,options,applicationconfig,newaccountwizard_u,registrierung,nagform,bbdebugtools,bibtexexport;

procedure openAboutDialog(title,description,creditlist: pchar; modal: boolean; game: longint);stdcall;external 'bbabout.dll';

procedure hideTaskBarIcon();
begin
  if not IsWindowVisible(GetWindow(mainform.handle,GW_OWNER)) then exit;
  ShowWindow(GetWindow(mainform.handle,GW_OWNER),SW_HIDE); //windows, win32
end;

procedure showTaskBarIcon();
begin
  ShowWindow(GetWindow(mainform.handle,GW_OWNER),SW_SHOW); //windows, win32
end;

function getListItemColor(item:TTreeListItem):TColor;
begin
  if Item.tag = 0 then exit(colorTimeNear);
  if PBook(Item.tag)^.actuality in [bltInOldAdd,bltInOldData] then
    result:=colorOld
  else if PBook(Item.tag)^.limitDate<=redTime then
    result:=colorTimeNear
  else if PBook(Item.tag)^.status in BOOK_NOT_EXTENDABLE then
    result:=colorLimited
  else result:=colorOK;
end;

procedure TmainForm.ListView1WindowProc(var TheMessage: TLMessage);
begin
end;

procedure TmainForm.searchInListView(step: integer; researchCurrentLine: boolean);
  procedure showSearchMark;
  const DT_WORD_ELLIPSIS           =$00040000;
        text_padding=3;
  var rec:trect;
      item:TTreeListItem;
      itemtext:string;
  begin
    item:=BookList.items[searchMarkRow];
    itemtext:=item.RecordItemsText[searchMarkCol];


    rec:=item.getBounds(searchMarkCol);

   // ListView1.Canvas.font:=ListView1.font;
    BookList.Canvas.brush.style:=bsSolid;
    BookList.Canvas.brush.color:=colorSearchMarkField; //getListItemColor(item);
  //  BookList.Canvas.brush.style:=bsClear;
    BookList.Canvas.fillrect(rec);

//    BookList.Canvas.TextRect(rec,rec.left+6,rec.top,copy(itemtext,1,searchMarkStart-1));
    if searchMarkStart-1>0 then
      DrawText(BookList.Canvas.handle,@copy(itemtext,1,searchMarkStart-1)[1],searchMarkStart-1,rec,DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_WORD_ELLIPSIS);
    rec.left:=rec.left+BookList.Canvas.TextWidth(copy(itemtext,1,searchMarkStart-1));
    if rec.left>rec.right then exit;
    BookList.Canvas.brush.color:=colorSearchMark;
    BookList.Canvas.fillrect(rec);
    if searchMarkLen>0 then
      DrawText(BookList.Canvas.handle,@copy(itemtext,searchMarkStart,searchMarkLen)[1],searchMarkLen,rec,DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_WORD_ELLIPSIS);
    //BookList.Canvas.TextRect(rec,rec.left,rec.top,copy(itemtext,searchMarkStart,searchMarkLen));
    rec.left:=rec.left+BookList.Canvas.TextWidth(copy(itemtext,searchMarkStart,searchMarkLen));
    if rec.left>rec.right then exit;
    delete(itemtext,1,searchMarkStart+searchMarkLen-1);
    BookList.Canvas.brush.color:=colorSearchMarkField;//getListItemColor(item);
    BookList.Canvas.fillrect(rec);
    if length(itemtext)>0 then
      DrawText(BookList.Canvas. handle,@itemtext[1],length(itemtext),rec,DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_WORD_ELLIPSIS);
//    BookList.Canvas.TextRect(rec,rec.left,rec.top,itemtext);
  {  defaultdraw:=false;
  //    BookList.OnCustomDrawSubItem:=nil;
    end else defaultdraw:=true;}
  end;

var st: string;

  function checkStr(row,col: integer):boolean;
  var stp:integer;
      currentText:string;
  begin
    result:=false;
    currentText:=BookList.items[row].RecordItemsText[col];
    stp:=pos(st,lowercase(currentText));
    if stp>0 then begin
      if searchMarkVisible and researchCurrentLine and (row=searchMarkRow) and
         ((col<searchMarkCol)or((col=searchMarkCol)and(stp<searchMarkStart))) then
           exit;
      if searchMarkVisible and ((searchMarkRow<>row)or(searchMarkCol<>col)or(searchMarkStart<>stp)) then
        BookList.Paint;

      searchMarkRow:=row;
      searchMarkCol:=col;
      searchMarkStart:=stp;
      searchMarkLen:=length(st);
      searchMarkVisible:=true;
      BookList.ensureVisibility(BookList.items[row]);


      showSearchMark;
      searchStatus.caption:='Gefunden: '+currentText;
      searchText.color:=colorSearchTextFound;
      result:=true;
    end;
  end;
var sp,ss,i: longint;
    field: integer;
begin
  searchStatus.caption:='';
  ss:=searchMarkRow;
  if ss<0 then ss:=0;
  st:=lowercase(searchText.Text);
  if researchCurrentLine then ss:=ss-step;
  if ss>=BookList.items.count then sp:=0;
  if ss<0 then ss:=BookList.items.count-1;
  sp:=ss;
  field:=searchField.itemIndex-2;
//  searchMarkVisible:=false;
  repeat
    sp:=sp+step;
    if sp>=BookList.items.count then begin
      sp:=0;
      searchStatus.caption:='Listenende erreicht, Suche wird am Anfang fortgesetzt';
    end;
    if sp<0 then begin
      sp:=BookList.items.count-1;
      searchStatus.caption:='Listenanfang erreicht, Suche wird am Ende fortgesetzt';
    end;
    case field of
      -2:  //every
        for i:=0 to BookList.items[sp].RecordItems.count-1 do
          if checkStr(sp,i) then exit;
      -1: if  checkStr(sp,2) or checkStr(sp,3)then exit; //author/title
      else if checkStr(sp,field) then exit;
    end;
  until sp=ss;
  if searchMarkVisible then begin
    BookList.Paint;
    searchMarkVisible:=false;
    searchStatus.caption:='Nicht gefunden';
    searchText.color:=colorSearchTextNotFound;
  end;
end;

procedure TmainForm.FormCreate(Sender: TObject);
//const IMAGE_FILES:array[0..2] of string=('refresh','homepages','options');

type TDeactivateWindowTheme= function (hWnd: Long;pszSubAppName: pwchar;pszSubIdList: pwchar): Long;stdcall; //uxtheme.dll

const defaultorder:array[0..9] of longint=(0,1,2,3,LV_BOOK_COLUMNS_YEAR_ID,4,5,6,7,9);

var i:integer;
    tempItem:TMenuItem;
    img,mask: graphics.TBITMAP;
    order: array[0..100] of longint;
    DeactivateWindowTheme: TDeactivateWindowTheme;
begin
  if logging then log('FormCreate started');
  

  Application.OnMinimize:=@appMinimize;
  Application.OnRestore:=@appRestore;
  
  TranslateUnitResourceStrings('LCLStrConsts',dataPath+'lclstrconsts.de.po');
  
//  ListView1.Column[LV_BOOK_COLUMNS_YEAR_ID].Index:=LV_BOOK_COLUMNS_YEAR_INDEX;


  
  searchMarkRow:=0;
  searchMarkVisible:=false;

  //ListView1.DoubleBuffered:=true;

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
  
  for i:=0 to accountIDs.count-1 do
    addGUIItemsForNewAccount(TCustomAccountAccess(accountIDs.Objects[i]));

  BookList:=TTreeListView.create(self);
  BookList.Parent:=self;
  BookList.Align:=alClient;
  BookList.RootLineMode:=lmNone;
  BookList.BackGroundColor:=colorOK;
  BookList.Columns.Clear;
  BookList.Striped:=false;
  BookList.multiSelect:=true;
  BookList.PopupMenu:=bookPopupMenu;
  BookList.OnSelect:=@BookListSelectItem;
  BookList.OnUserSortItemsEvent:=@BookListUserSortItemsEvent;
  BookList.OnCompareItems:=@BookListCompareItems;
  BookList.OnCustomItemDraw:=@BookListCustomItemDraw;
  BookList.ColumnsDragable:=true;
  with BookList.Columns.Add do begin
    Text:='ID';
    Width:=80;
  end;
  with BookList.Columns.Add do begin
    Text:='Kategorie';
    Width:=50;
  end;
  with BookList.Columns.Add do begin
    Text:='Verfasser';
    Width:=120;
  end;
  with BookList.Columns.Add do begin
    Text:='Titel';
    Width:=150;
  end;
  with BookList.Columns.Add do begin
    Text:='Ausleihe';
    Width:=70;
  end;
  with BookList.Columns.Add do begin
    Text:='Frist';
    Width:=70;
  end;
  with BookList.Columns.Add do begin
    Text:='Konto';
    Width:=80;
  end;
  with BookList.Columns.Add do begin
    Text:='Bemerkung';
    Width:=250;
  end;
           {//TODO: Header
  for i:=0 to BookList.Columns.Count-1 do begin
    ListView1.Columns[i].Width:=userConfig.ReadInteger('BookList','ColumnWidth'+IntToStr(i),
                                                        ListView1.Columns[i].Width);
    order[i]:=userConfig.ReadInteger('BookList','ColumnPos'+IntToStr(i),defaultorder[i]);
  end;    }

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
    img.LoadFromFile(programPath+'images\'+IMAGE_FILES[i]+'.bmp');
 //   mask.LoadFromFile(programPath+'images\'+IMAGE_FILES[i]+'_mask.bmp');
    ImageList1.Add(img,nil)
  end;       }

  caption:=appFullTitle;

  {$ifdef debug}
  caption:=caption+' (DEBUG-MODE!)';
  {$endif}


    //image1.Picture.LoadFromFile(programPath+'images\'+IMAGE_FILES[0]+'.bmp');
  if logging then log('FormCreate ende');
end;

procedure TmainForm.FormDestroy(Sender: TObject);
begin
  BookList.Free;
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

procedure TmainForm.FormShow(Sender: TObject);
begin
end;

procedure TmainForm.FormWindowStateChange(Sender: TObject);
begin
end;


procedure TmainForm.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
end;


procedure TmainForm.Button2Click(Sender: TObject);
begin
end;

procedure TmainForm.Button1Click(Sender: TObject);
begin
end;

procedure TmainForm.bookPopupMenuPopup(Sender: TObject);
begin
{  extendThisBooks.Enabled:=(ListView1.SelCount=1) and
                  (PBook(ListView1.Selected.Data))^.lib.getLibrary().canModifySingleBooks;}
  extendTheseBooks.Enabled:=(BookList.SelCount>=1);
  extendAdjacentBooks.Enabled:=BookList.SelCount=1;
end;

procedure TmainForm.appMinimize(Sender: TObject);
begin
  if startToTNA then
    hideTaskBarIcon();
end;

procedure TmainForm.appRestore(Sender: TObject);
begin
  if startToTNA then
    showTaskBarIcon();
end;

procedure TmainForm.BookListCompareItems(sender: TObject; i1,
  i2: TTreeListItem; var compare: longint);
var book1,book2: PBook;
begin
  book1:=PBook(i1.Tag);
  book2:=PBook(i2.Tag);
  if (book1=nil) then begin
    Compare:=1;
    exit;
  end;
  if (book2=nil) then begin
    Compare:=-1;
    exit;
  end;
  compare:=0;
  case BookList.SortColumn of
    4: if book1^.issueDate<book2^.issueDate then
         compare:=-1
       else if book1^.issueDate>book2^.issueDate then
         compare:=1;
    5:; //see later

    else compare:=CompareText(i1.RecordItemsText[BookList.SortColumn],i2.RecordItemsText[BookList.SortColumn]);
  end;
  if compare=0 then  //Sort LimitDate
    if book1^.limitDate<book2^.limitDate then
       compare:=-1
    else if book1^.limitDate>book2^.limitDate then
       compare:=1;
  if compare=0 then       //Sort Status
    if (book1^.status in BOOK_NOT_EXTENDABLE) and (book2^.status in BOOK_EXTENDABLE) then
      compare:=-1
     else if (book1^.status in BOOK_EXTENDABLE) and (book2^.status in BOOK_NOT_EXTENDABLE) then
      compare:=1;
//     else compare:=compareText(PBook(item1.data)^.statusStr,PBook(item2.data)^.statusStr);
  if compare=0 then       //Sort ID
    compare:=compareText(i1.Text,i2.Text);
end;

procedure TmainForm.BookListCustomItemDraw(sender: TObject;
  eventTyp_cdet: TCustomDrawEventTyp; item: TTreeListItem; xpos, ypos,
  xColumn: integer; lastItem: boolean; var defaultDraw: Boolean);
begin
  if not item.SeemsSelected then begin
    BookList.canvas.Brush.Style:=bsSolid;
    BookList.Canvas.brush.color:=getListItemColor(item);
  end;
  defaultDraw:=true;
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

procedure TmainForm.delayedPopupTimer(Sender: TObject);
begin
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
      until (accountIDs.count<>0) or (MessageBox(0,'Sie müssen ein Konto angeben, um VideLibri benutzen zu können.'#13#10'Wollen Sie eines eingeben?','VideLibri',MB_YESNO or MB_ICONQUESTION)=mrNo);
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
var i:longint;
    order:array[0..10] of longint;
begin
  if windowstate<>wsMaximized then begin
    userConfig.WriteInteger('window','left',left);
    userConfig.WriteInteger('window','top',top);
    userConfig.WriteInteger('window','width',width);
    userConfig.WriteInteger('window','height',height);
  end;
  if windowstate<>wsMinimized then
    userConfig.writeinteger('window','state',integer(windowstate));


  {SendMessageA(ListView1.Handle,LVM_GETCOLUMNORDERARRAY,ListView1.Columns.Count,lparam(@order[0]));
  for i:=0 to ListView1.Columns.Count-1 do begin
    userConfig.WriteInteger('BookList','ColumnWidth'+IntToStr(i),
                             ListView1.Columns[i].Width);
    userConfig.WriteInteger('BookList','ColumnPos'+IntToStr(i),order[i]);
  end; }
end;

procedure TmainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TmainForm.ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
end;

procedure TmainForm.ListView1CustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
end;


procedure TmainForm.ListView1CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
end;

procedure TmainForm.ListView1CustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
end;

procedure TmainForm.ListView1MouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
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

procedure TmainForm.MenuItem11Click(Sender: TObject);
var i,j:integer;
    books:TBookArray;
begin
  setlength(books,BookList.SelCount);
  j:=0;
  for i:=0 to BookList.Items.Count-1 do begin
    if BookList.Items[i].Selected then begin
      books[j]:=PBook(BookList.Items[i].Tag);
      inc(j);
    end ;
  end;
  extendBooks(books);
end;

procedure TmainForm.MenuItem14Click(Sender: TObject);
begin
  WinExec('hh -mapid 1000 videlibri.chm',sw_shownormal);
end;

procedure TmainForm.MenuItem15Click(Sender: TObject);
begin
  WinExec('hh -mapid 1001 videlibri.chm',sw_shownormal);
end;

procedure TmainForm.MenuItem16Click(Sender: TObject);
begin
  TsendBackErrorForm.openErrorWindow(oldErrorMessageString,IntToStr(versionNumber),'Videlibri');
end;

procedure TmainForm.MenuItem17Click(Sender: TObject);
begin
  openAboutDialog(pchar({'Videlibri '+FloatToStr(versionNumber/1000)}caption),'',
   'Verwendete Fremdkomponenten:'#13#10'LCL'#13#10'FreePascal Runtime'#13#10'TRegExpr von Andrey V. Sorokin'#13#10#13#10+
   'Verwendete Entwicklungswerkzeuge:'#13#10'Lazarus'#13#10'FreePascal'#13#10'GIMP'#13#10'HTML Help Workshop'#13#10'The Regex Coach',true,1);
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
  bibexportForm.ShowModal;
  bibexportForm.free;
end;

procedure TmainForm.UserExtendMenuClick(Sender: TObject);
var limitTime: longint;
begin
  limitTime:=(sender as tmenuItem).Parent.tag;
  extendBooks(limitTime+currentDate,TCustomAccountAccess(tmenuitem(sender).tag));
end;

procedure TmainForm.MenuItem19Click(Sender: TObject);
begin

end;

procedure TmainForm.MenuItem2Click(Sender: TObject);
begin
end;

procedure TmainForm.MenuItem3Click(Sender: TObject);
begin
  close;
end;

procedure TmainForm.MenuItem4Click(Sender: TObject);
begin
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
  searchPanel.show;
  searchMarkRow:=0;
  searchMarkCol:=0;
  searchText.SetFocus;
end;

procedure TmainForm.Panel1MouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TmainForm.searchCloseClick(Sender: TObject);
begin
  searchPanel.visible:=false;
end;

procedure TmainForm.searchDownClick(Sender: TObject);
begin

end;

procedure TmainForm.searchTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    13: begin
      key:=0;
      if ssShift in shift then searchInListView(-1)
      else searchInListView(1);
    end;
    vk_escape: searchClose.click;
    else searchInListView(1,true);
  end;
end;

procedure TmainForm.searchUpClick(Sender: TObject);
begin
  if sender=searchUp then searchInListView(-1)
  else searchInListView(1)
end;

procedure TmainForm.showAccount(Sender: TObject);
var i:integer;
begin
  assert(sender is TMenuItem);
  if (sender =showAllAccounts) or (sender=showNoAccounts) then begin
    for i:=0 to viewMenu.Count-1 do
      if (viewMenu.Items[i].tag <> 0) and (viewMenu.Items[i].tag <> -1) then
        viewMenu.items[i].Checked:=sender=showAllAccounts;
  end else
    TMenuItem(sender).Checked:=not TMenuItem(sender).Checked;
  RefreshListView;
end;

procedure TmainForm.refreshAccountClick(Sender: TObject);
begin
  updateAccountBookData(TCustomAccountAccess((sender as tmenuitem).tag),false,false,false);
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
    if not lib.allowHomepageNavigation then extraParams:=extraParams+' /no-navigation';
    if lib.bestHomepageWidth>0 then extraParams:=extraParams+' /pagewidth='+InttoStr(lib.bestHomepageWidth);
    if lib.bestHomepageHeight>0 then extraParams:=extraParams+' /pageheight='+InttoStr(lib.bestHomepageHeight);
  end;
//  ShowMessage(TMenuItem(sender).Caption);
  if (userConfig.ReadInteger('access','homepage-type',1)=0) and (FileExists(programPath+'simpleBrowser.exe')) then
     WinExec(pchar(programPath+'simpleBrowser /title="'+title+'" /site="'+baseURL+'" /configfile="'+userPath+'browserconfig.ini" /windowsection='+id+' '+extraParams),SW_SHOWNORMAL)
   else
     shellexecute(handle,'open',pchar(baseURL),'','',SW_SHOWNORMAL);
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
               with TCustomAccountAccess(accountIDs.Objects[j]) do
                 s:=s+#13#10'        '+prettyName+': '+DateToPrettyStr(lastCheckDate);
             ShowMessage(s);
           end;
      end;
    end else dec(pos.x,statusbar1.panels.items[i].Width);
end;

procedure TmainForm.Timer1Timer(Sender: TObject);
begin
  if startToTNA and lclStarted then
    if // {and (GetTickCount-lclStartedTime>5000) and} and
    (windowstate=wsMinimized) or (not IsWindowVisible(handle)) then // then
    hideTaskBarIcon()
   else
    showTaskBarIcon();
  if needApplicationRestart and IsWindowEnabled(handle)and IsWindowVisible(handle) then
    close;
end;

procedure TmainForm.ToolBar1Click(Sender: TObject);
begin
end;

procedure TmainForm.ToolBar1Resize(Sender: TObject);
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
    book:PBook;
    sharewaretest:boolean; //für Sharewaretest
    books:TBookLists;
    typ: TBookOutputType;
    criticalSessionUsed:boolean;
    maxcharges:currency;
var ok1,ok2,ok3,nok1,nok2,nok3: longbool; //für Sharewaretest
    user,code:string;                                      //für Sharewaretest
label showOnly10;                  //für Sharewaretest
{$include _shareware.inc}
begin
  if logging then log('RefreshListView started');
  lastCheck:=currentDate;
  if ViewAll.Checked then typ:=botAll
  else if ViewOld.Checked then typ:=botOld
  else typ:=botCurrent;
  criticalSessionUsed:=false;
  if UpdateThreadsRunning>0 then begin
    EnterCriticalSection(updateThreadConfig.libraryAccessSection);
    criticalSessionUsed:=true;
  end;
  BookList.BeginUpdate;
  BookList.items.clear;
  for i:=0 to viewMenu.Count-1 do begin
    if (viewMenu.Items[i].tag = 0) or (viewMenu.Items[i].tag = -1) then continue;
    if not viewMenu.Items[i].Checked then continue;
    books:=///TCustomAccountAccess(accountIDs.objects[viewMenu.Items[i].Tag]).getBooks();
           TCustomAccountAccess(viewMenu.Items[i].Tag).getBooks();
    for j:=0 to books.getBookCount(typ)-1 do begin
      book:=books.getBook(typ,j);
      with BookList.items.Add do begin
        text:=book^.id;
        RecordItems.Add(Utf8ToAnsi(book^.category));
        RecordItems.Add(Utf8ToAnsi(book^.author));
        RecordItems.Add(Utf8ToAnsi(book^.title));
        RecordItems.Add(DateToPrettyStr(book^.issueDate));
        if book^.actuality in [bltInOldData,bltInOldAdd] then
         RecordItems.Add('erledigt')
        else
         RecordItems.Add(DateToPrettyStr(book^.limitDate));
        RecordItems.Add(book^.lib.prettyName);
        if book^.actuality in [bltInOldData,bltInOldAdd] then
          RecordItems.Add('')
         else begin
           case book^.Status of
            bsNormal: RecordItems.Add('');
            bsUnknown: RecordItems.Add('Ausleihstatus unbekannt');
            bsIsSearched: RecordItems.Add('Ausleihstatus wird ermittelt... (sollte nicht vorkommen, bitte melden!)');
            bsEarMarked:RecordItems.Add('vorgemerkt');
            bsMaxLimitReached: RecordItems.Add('maximale Ausleihfrist erreicht');
            bsAccountExpired: RecordItems.Add('Büchereikarte ist abgelaufen');
            bsProblematicInStr,bsCuriousInStr: RecordItems.Add(Utf8ToAnsi(book^.statusStr));
            else RecordItems.Add('Unbekannter Fehler! Bitte melden!');
           end;
           lastCheck:=min(lastCheck,book^.lastExistsDate);
        end;
        RecordItems.Add(Utf8ToAnsi(book^.year)); ;
       // SubItems.add(book^.otherInfo);
        Tag:=longint(book);
      end;
    end;
  end;
  BookList.Sorted:=true;

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
    if TCustomAccountAccess(viewMenu.Items[i].Tag).charges>0 then
      maxcharges:=maxcharges+TCustomAccountAccess(viewMenu.Items[i].Tag).charges;
  end;

  //SHAREWARE CODE
  while ok2<>ok1 do maxcharges:=maxcharges+2;
  if nok1 = ok1 then close;
  //SHAREWARE CODE END

  if maxcharges>0 then begin
    StatusBar1.Panels[1].text:='Offene Gebühren: '+floattostr(maxcharges)+'€ (';
    for i:=0 to viewMenu.Count-1 do begin
      if (viewMenu.Items[i].tag = 0) or (viewMenu.Items[i].tag = -1) then continue;
      if TCustomAccountAccess(viewMenu.Items[i].Tag).charges>0 then
        StatusBar1.Panels[1].text:=StatusBar1.Panels[1].text+
          TCustomAccountAccess(viewMenu.Items[i].Tag).prettyName+': '+
          FloatToStr((TCustomAccountAccess(viewMenu.Items[i].Tag).charges)) +'€ ';
    end;
    StatusBar1.Panels[1].text:=StatusBar1.Panels[1].text+')';
  end else StatusBar1.Panels[1].text:='';
  StatusBar1.Panels[SB_PANEL_COUNT].Text:='Medien: '+IntToStr(BookList.Items.Count);

  //SHAREWARE CODE
  {$I obfuscate.inc}
  if BookList.Items.count>10 then
    if nok2 then halt;
  //SHAREWARE CODE END

  if updateThreadsRunning<=0 then
      if abs(lastCheck-currentDate)>2 then
        StatusBar1.Panels[0].text:='Älteste angezeigte Daten sind vom '+DateToStr(lastCheck)
       else
        StatusBar1.Panels[0].text:='Älteste angezeigte Daten sind von  '+DateToPrettyStr(lastCheck);
  icon.LoadFromFile(getTNAIconFileName());

  //SHAREWARE CODE
  //Bücherzahl überprüfen

  BookList.EndUpdate;

  count:=0;
  count2:=0;
  for i:=0 to viewMenu.Count-1 do begin
    if (viewMenu.Items[i].tag = 0) or (viewMenu.Items[i].tag = -1) then continue;
  //  if not viewMenu.Items[i].Checked then continue;
    books:=///TCustomAccountAccess(accountIDs.objects[viewMenu.Items[i].Tag]).getBooks();
           TCustomAccountAccess(viewMenu.Items[i].Tag).getBooks();
    count+=books.getBookCount(typ);
    if viewMenu.Items[i].Checked then count2+=books.getBookCount(typ);
  end;
  StatusBar1.Panels[SB_PANEL_COUNT].Text:=StatusBar1.Panels[SB_PANEL_COUNT].Text+'/'+inttostr(count);
  if count2>BookList.Items.count then begin
    with BookList.Items.Add do begin
      text:='ACHTUNG';
      RecordItems.Add('');
      RecordItems.Add('VideLibri');
      RecordItems.Add('weitere '+IntToStr(count-BookList.items.count+1)+' Medien ausgeblendet');
      RecordItems.Add('');
      RecordItems.Add('');
      RecordItems.Add('');
      RecordItems.Add('Nur die Vollversion zeigt mehr als 10 Medien an.');
      RecordItems.Add('');
      //SubItems.add('');
      Tag:=0;
    end;
  end else if count2>10 then
    if  (nok1 or not ok3) then close;
  //SHAREWARE CODE ENDE


  if criticalSessionUsed then
    LeaveCriticalSection(updateThreadConfig.libraryAccessSection);

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


procedure TmainForm.addGUIItemsForNewAccount(const account: TCustomAccountAccess);

  function newItem(owner: TComponent;onClick:TNotifyEvent):TMenuItem;
  begin
    newItem:=TMenuItem.Create(owner);
    newItem.Caption:=account.prettyName;
    newItem.tag:=longint(account);
    newItem.OnClick:=onclick;
  end;
  procedure addToMenuItem(item: TMenuItem;onClick:TNotifyEvent);
  begin
    item.add(newItem(item,onclick));
  end;
var temp:TMenuItem;
    i:integer;

begin
  accountListMenu.Items.Add(newItem(accountListMenu,@refreshAccountClick));
  accountListMenuItem.Add(newItem(accountListMenu,@refreshAccountClick));
  temp:=newItem(accountListMenu,@showAccount);
  temp.Checked:=true;
  viewMenu.Add(temp);
  addToMenuItem(extendMenuList1,@UserExtendMenuClick);
  for i:=0 to extendMenuList2_.Count-1 do
    addToMenuItem(extendMenuList2_[i],@UserExtendMenuClick);
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
  procedure checkMenuItem(item:TMenuItem);
  var i:integer;
  begin
    for i:=item.Count-1 downto 1 do
      if item.items[i].tag=longint(account) then item.Delete(i)
  end;

var i:integer;
begin
  if account=nil then exit;
  checkMenuItem(accountListMenuItem);
  checkMenuItem(viewMenu);
  checkMenuItem(extendMenuList1);
  for i:=extendMenuList2_.count-1 downto 0 do
    checkMenuItem(extendMenuList2_[i]);
  for i:=accountListMenu.items.count-1 downto 0 do
    if accountListMenu.items[i].tag=longint(account) then begin
      accountListMenu.items.Delete(i);
      break;
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

  addGUIItemsForNewAccount(result);

  accountIDs.AddObject(result.getID(),result);
  result.save();
  saveLibIDs;

{  if MessageDlg('Daten laden?',
                'Das Konto '+lib.getPrettyName()+' wurde erstellt.'#13#10'Sollen jetzt die Mediendaten heruntergeladen werden?',
                mtConfirmation ,[mbYes,mbNo],0)=mrYes then
    mainForm.updateLibrary(lib,false,false);}
end;

procedure TmainForm.lm_rbuttonup(var mes: TLMRButtonUp);
begin
end;

procedure TmainForm.ThreadDone(Sender: TObject);
//called in the main thread
begin
  libraryAccess.threadDone(Sender); //self=nil !!!

end;


initialization
  {$I bookwatchmain.lrs}
end.
