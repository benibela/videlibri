unit librarySearcherAccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils , librarySearcher,libraryParser, booklistreader,bbutils,messagesystem,simplexmlparser,multipagetemplate;

type

TBookNotifyEvent=procedure (sender: TObject; book: TBook) of object;
TPageCompleteNotifyEvent=procedure (sender: TObject; firstPage, nextPageAvailable: boolean) of object;
TPendingMessageEvent=procedure (sender: TObject; book: TBook; pendingMessage: TPendingMessage) of object;
TLibrarySearcherAccess = class;

{ TLibrarySearcherAccess }

TSearcherMessageTyp=(smtNone, smtFreeSearcher, smtFreeSearcherAndAccess, smtConnect, smtSearch, smtSearchNext, smtDetails, smtOrder, smtCompletePendingMessage, smtDisconnect);

{ TSearcherMessage }

TSearcherMessage = class
  typ: TSearcherMessageTyp;
  book: tbook;
  pendingMessage: TPendingMessage;
  messageResult: integer;
  constructor create(t:TSearcherMessageTyp; b:TBook=nil; apendingMessage: TPendingMessage = nil; mesresult: integer = 0);
end;

{ TSearcherThread }

TSearcherThread = class(TThread)
private
  fbookAccessSection: TRTLCriticalSection;

  access: TLibrarySearcherAccess;
  messages: TMessageSystem;
  Searcher: TLibrarySearcher;
  
  notifyEvent: TNotifyEvent;
  bookNotifyEvent: TBookNotifyEvent;
  pageCompleteEvent: TPageCompleteNotifyEvent;
  pendingMessageEvent: TPendingMessageEvent;
  //connectedEvent: TNotifyEvent;
  changedBook: tbook;
  firstPageForSync, nextPageAvailableForSync: boolean;
  pendingMessage: TPendingMessage;

  performingAction: TSearcherMessageTyp;

  procedure desktopSynchronized(t: TThreadMethod);

  procedure callNotifyEventSynchronized;
  procedure callBookEventSynchronized;
  procedure callPageCompleteEventSynchronized;
  procedure callPendingMessageEventSynchronized;

  procedure callNotifyEvent(event: TNotifyEvent);
  procedure callBookEvent(event: TBookNotifyEvent; book: tbook);
  procedure callPageCompleteEvent(event: TPageCompleteNotifyEvent; firstPage, nextPageAvailable: boolean);
  procedure callPendingMessageEvent(event: TPendingMessageEvent; book: TBook; pendingMes: TPendingMessage);

  procedure processPendingMessage(book: TBook; isOrderingAction: boolean);
protected
  procedure execute;override;
public
  constructor create(template: TMultiPageTemplate; aaccess: TLibrarySearcherAccess);
  destructor destroy;override;
end;


TLibrarySearcherAccess = class
private
  FOnException: TNotifyEvent;
  fthread: TSearcherThread;
  ftemplate: TMultiPageTemplate;
  FOnConnected, FOnPendingMessageCompleted: TNotifyEvent;
  FOnDetailsComplete, FOnOrderComplete: TBookNotifyEvent;
  FOnSearchPageComplete: TPageCompleteNotifyEvent;
  FOnTakePendingMessage: TPendingMessageEvent;

  procedure removeOldMessageOf(typ: TSearcherMessageTyp);
  function GetSearcher: TLibrarySearcher;
  procedure loadSearchCache;
public
  function operationActive: boolean;

  constructor create();

  procedure beginBookReading; //to access any searched book
  procedure endBookReading;
  procedure beginResultReading; //before accessing searcher
  procedure endResultReading;


  procedure newSearch(template: TMultiPageTemplate); //ensures that all operations are finished
  procedure prepareNewSearchWithoutDisconnect; //ensures that all operations are finished, without changing the connection state

  procedure connectAsync;
  procedure searchAsync;
  procedure searchNextAsync;
  procedure detailsAsyncSave(book: TBook); //save means they make sure the
  procedure orderAsync(account: TCustomAccountAccess; book: TBook);
  procedure completePendingMessage(book: TBook; pm: TPendingMessage; result: integer);
  procedure disconnectAsync;
  procedure freeAsync;

  property OnException: TNotifyEvent read FOnException write FOnException;
  property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
  property OnSearchPageComplete: TPageCompleteNotifyEvent read FOnSearchPageComplete write FOnSearchPageComplete;
  property OnDetailsComplete: TBookNotifyEvent read FOnDetailsComplete write FOnDetailsComplete;
  //holding order events:
  //after calling orderAsync,
  //                  --if there is a message---> OnTakePendingMessage  --\
  //                  --if there is no message--> OnOrderComplete         |
  //                                                                      |
  //             /--------------------------------------------------------+
  //            \|/                                                       |
  //after calling completePendingMessage                                  |
  //                 --if there is a message---> OnTakePendingMessage ----/
  //                 --if there is no message
  //                          -----if status=ordered-----> OnOrderComplete
  //                          -----if status<>ordered----> OnPendingMessageCompleted
  property OnOrderComplete: TBookNotifyEvent read FOnOrderComplete write FOnOrderComplete;
  property OnTakePendingMessage: TPendingMessageEvent read FOnTakePendingMessage write FOnTakePendingMessage;
  property OnPendingMessageCompleted: TNotifyEvent read FOnPendingMessageCompleted write FOnPendingMessageCompleted;
  property searcher: TLibrarySearcher read GetSearcher;
end;

type

  { TSearchTarget }

  TSearchTarget = class
    id: string;
    name: string;
    lib: TLibrary; //might be nil
    template: TMultiPageTemplate;
    constructor create(aname: string; alib: TLibrary; atemplate: TMultiPageTemplate);
    constructor create(aid: string);
  end;

  { TSearchableLocations }

  TSearchableLocations = record
    regions: TStringList; //string -> tstringlist
    locations:  TStringList; //string->stringlist of TSearchTarget
    //searchTemplates: TStringList;
  end;

function getSearchableLocations: TSearchableLocations;
implementation

uses applicationconfig, bbdebugtools, internetaccess, androidutils {$ifdef android}, bbjniutils{$else},forms{$endif}, commoninterface, xquery, jsonscannerhelper;

resourcestring
  rsSearchAllRegions = 'alle Regionen';

function getSearchableLocations: TSearchableLocations;
  function nestedList(base: TStringList; name: string): TStringList;
  var
    i: Integer;
  begin
    i := base.IndexOf(name);
    if i < 0 then begin
      result := TStringList.Create;
      result.OwnsObjects := true;
      i := base.AddObject(name, result);
    end;
    result := TStringList(base.Objects[i]);
  end;

var
  temp: TStringList;
  loc: String;
  libraryMetaData: TLibraryMetaDataEnumerator;
begin
  with result do begin
    regions := TStringList.Create;
    regions.OwnsObjects := true;
    locations := TStringList.Create;
    locations.OwnsObjects := true;


    libraryMetaData := libraryManager.enumerateLibraryMetaData;
    while libraryMetaData.MoveNext do begin
      loc := libraryMetaData.prettyLocation;
      temp := nestedList(regions, libraryMetaData.prettyCountryState);
      if temp.IndexOf(loc) < 0 then temp.Add(loc);
      temp := nestedList(locations, loc);
      temp.AddObject(libraryMetaData.libraryId, TSearchTarget.create(libraryMetaData.libraryId));
    end;

    locations.Sort;
    nestedList(regions, rsSearchAllRegions).Text := locations.Text;
    regions.Sort;
  end;
end;

{ TSearchTarget }

constructor TSearchTarget.create(aname: string; alib: TLibrary; atemplate: TMultiPageTemplate);
begin
  name := aname;
  lib := alib;
  template := atemplate;
end;

constructor TSearchTarget.create(aid: string);
begin
  id := aid
end;

{ TLibrarySearcherAccess }

function TLibrarySearcherAccess.GetSearcher: TLibrarySearcher;
begin
  if not assigned(fthread) then exit(nil);
  Result:=fthread.Searcher;
end;



constructor TLibrarySearcherAccess.create();
begin
end;

procedure TLibrarySearcherAccess.freeAsync;
begin
  if self = nil then exit;
  if assigned(fthread) then fthread.messages.storeMessage(TSearcherMessage.Create(smtFreeSearcherAndAccess))
  else free;
end;


procedure TLibrarySearcherAccess.beginBookReading;
begin
  if not assigned(fthread) then exit;
  EnterCriticalsection(fthread. fbookAccessSection);
end;

procedure TLibrarySearcherAccess.endBookReading;
begin
  if not assigned(fthread) then exit;
  LeaveCriticalsection(fthread.fbookAccessSection);
end;

procedure TLibrarySearcherAccess.beginResultReading;
begin
  beginBookReading;
end;

procedure TLibrarySearcherAccess.endResultReading;
begin
  endBookReading;
end;

function TLibrarySearcherAccess.operationActive: boolean;
begin
  ReadBarrier;
  result:=assigned(fthread) and (fthread.messages.existsMessage or (fthread.performingAction <> smtNone));
end;

procedure TLibrarySearcherAccess.removeOldMessageOf(typ: TSearcherMessageTyp);
var list: TFPList;
    i:longint;
begin
  if not assigned(fthread) then exit;
  list:=fthread.messages.openDirectMessageAccess;
  for i:=list.Count-1 downto 0 do
    if TSearcherMessage(list[i]).typ=typ then begin
      tobject(list[i]).free;
      list.Delete(i);
    end;
  fthread.messages.closeDirectMessageAccess(list);
end;

procedure TLibrarySearcherAccess.newSearch(template: TMultiPageTemplate);
begin
  if (operationActive) or not Assigned(fthread) or (ftemplate <> template) then begin
    if assigned(fthread) then fthread.messages.storeMessage(TSearcherMessage.Create(smtFreeSearcher));
    ftemplate := template;
    fthread:=TSearcherThread.Create(ftemplate,self);
    ReadWriteBarrier;
    while fthread.Searcher = nil do begin sleep(25); ReadBarrier; end; //wait for thread start
    ReadBarrier;
  end;
end;

procedure TLibrarySearcherAccess.prepareNewSearchWithoutDisconnect;
const ALLOWED_ACTIONS = [smtNone, smtFreeSearcher, smtFreeSearcherAndAccess, smtConnect, smtDisconnect];
var
  list: TFPList;
  i: Integer;
begin
  list:=fthread.messages.openDirectMessageAccess;
  for i:=list.Count-1 downto 0 do
    if not (TSearcherMessage(list[i]).typ in ALLOWED_ACTIONS) then begin
      tobject(list[i]).free;
      list.Delete(i);
    end;
  fthread.messages.closeDirectMessageAccess(list);

  while assigned(fthread) and not (fthread.performingAction in ALLOWED_ACTIONS) do begin
    ReadWriteBarrier; //i do not understand what this does
    sleep(50);
    {$ifndef android}application.ProcessMessages;{$endif}
  end;
end;

procedure TLibrarySearcherAccess.loadSearchCache;
var
  cache: String;
  json, temp: IXQValue;
begin
  cache := strLoadFromFile(searcher.getCacheFile);
  if cache = '' then exit;
  try
    json := commoninterface.parseJSON(cache);
    temp := json.getProperty('search-params');
    if temp.kind = pvkObject then
      searcher.SearchParams := TFormParams.fromJSON(temp);
  except
    on e: Exception do
      storeException(e,nil,searcher.getLibraryIds,'-', nil);
  end;
end;


procedure TLibrarySearcherAccess.connectAsync;
begin
  if not assigned(fthread) then exit;
  removeOldMessageOf(smtConnect);
  if FileExists(searcher.getCacheFile) then loadSearchCache;
  fthread.messages.storeMessage(TSearcherMessage.Create(smtConnect));
end;

procedure TLibrarySearcherAccess.searchAsync;
begin
  if not assigned(fthread) then exit;
  removeOldMessageOf(smtSearch);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtSearch));
end;

procedure TLibrarySearcherAccess.searchNextAsync;
begin
  if not assigned(fthread) then exit;
  removeOldMessageOf(smtSearchNext);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtSearchNext));
end;

procedure TLibrarySearcherAccess.detailsAsyncSave(book: TBook);
begin
  if not assigned(fthread) then exit;
  removeOldMessageOf(smtDetails);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtDetails,book));
end;


procedure TLibrarySearcherAccess.orderAsync(account: TCustomAccountAccess; book: TBook);
begin
  if not assigned(fthread) then exit;
  book.owningAccount := account;
  fthread.messages.storeMessage(TSearcherMessage.Create(smtOrder,book));
end;

procedure TLibrarySearcherAccess.completePendingMessage(book: TBook; pm: TPendingMessage; result: integer);
begin
  if not assigned(fthread) then exit;
  if pm.callback = '' then exit;
  fthread.messages.storeMessage(TSearcherMessage.Create(smtCompletePendingMessage,book, pm, result));
end;

procedure TLibrarySearcherAccess.disconnectAsync;
begin
  if not assigned(fthread) then exit;
  removeOldMessageOf(smtDisconnect);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtDisconnect));
end;


{ TSearcherThread }

procedure TSearcherThread.desktopSynchronized(t: TThreadMethod);
begin
  {$ifndef android}
  Synchronize(t);
  {$else}
  t();
  {$endif}
end;

procedure TSearcherThread.callNotifyEventSynchronized;
begin
  if self<>access.fthread then exit;
  try
    notifyEvent(access);
  finally
    notifyEvent:=nil;
  end;
end;

procedure TSearcherThread.callBookEventSynchronized;
begin
  if self<>access.fthread then exit;
  try
    bookNotifyEvent(access,changedBook);
  finally
    changedBook := nil;
  end;
end;

procedure TSearcherThread.callPageCompleteEventSynchronized;
begin
  if self<>access.fthread then exit;
  if pageCompleteEvent = nil then exit;
  try
    pageCompleteEvent(access, firstPageForSync, nextPageAvailableForSync);
  finally
    pageCompleteEvent:=nil;
  end;
end;

procedure TSearcherThread.callPendingMessageEventSynchronized;
begin
  if self<>access.fthread then exit;
  try
    pendingMessageEvent(access,changedBook,pendingMessage);
  finally
    changedBook := nil;
    pendingMessage := nil;
  end;
end;


procedure TSearcherThread.callNotifyEvent(event: TNotifyEvent);
begin
  if event=nil then exit;
  while notifyEvent<>nil do sleep(5);
  notifyEvent:=event;
  desktopSynchronized(@callNotifyEventSynchronized);
end;

procedure TSearcherThread.callBookEvent(event: TBookNotifyEvent; book: tbook);
begin
  if event=nil then exit;
  if logging then log('Changed book: '+strFromPtr(changedBook));
  while changedBook<>nil do sleep(5);
  if logging then log('wait complete');
  bookNotifyEvent:=event;
  changedBook:=book;
  desktopSynchronized(@callBookEventSynchronized);
end;

procedure TSearcherThread.callPageCompleteEvent(event: TPageCompleteNotifyEvent; firstPage, nextPageAvailable: boolean);
begin
  if event = nil then exit;
  while pageCompleteEvent <> nil do sleep(5);
  pageCompleteEvent:=event;
  firstPageForSync:=firstPage;
  nextPageAvailableForSync:=nextPageAvailable;
  desktopSynchronized(@callPageCompleteEventSynchronized);
end;

procedure TSearcherThread.callPendingMessageEvent(event: TPendingMessageEvent; book: TBook; pendingMes: TPendingMessage);
begin
  if event=nil then exit;
  if logging then log('Pending message: '+strFromPtr(changedBook)+' '+pendingMes.caption);
  while changedBook<>nil do sleep(5);
  if logging then log('wait complete');
  pendingMessageEvent:=event;
  changedBook:=book;
  pendingMessage:=pendingMes;
  desktopSynchronized(@callPendingMessageEventSynchronized);
end;

procedure TSearcherThread.processPendingMessage(book: TBook; isOrderingAction: boolean);
var
  tmpPendingMessage: TPendingMessage;
  hasCallback: Boolean;
begin
  tmpPendingMessage := Searcher.bookListReader.pendingMessage;
  if tmpPendingMessage <> nil then begin
    hasCallback := tmpPendingMessage.callback <> '';
    callPendingMessageEvent(access.FOnTakePendingMessage, book, tmpPendingMessage);
    Searcher.bookListReader.pendingMessage := nil;
    if hasCallback then exit;  //the callback should be called by complete message, and handle it from there
  end;
  if isOrderingAction then begin
    callBookEvent(access.FOnOrderComplete, book);
  end else callNotifyEvent(access.FOnPendingMessageCompleted);
end;

procedure TSearcherThread.execute;
var mes: TSearcherMessage;
    book: tbook;
    debugLastSearchQuery: String;
    newsearcher: TLibrarySearcher;
begin
  newsearcher := TLibrarySearcher.create(access.ftemplate);
  newsearcher.bookListReader.bookAccessSection:=@fbookAccessSection;
  ReadWriteBarrier;
  Searcher := newsearcher;
  while true do begin
    try
      performingAction:=smtNone;
      if logging then log('Searcher thread: wait for message');
      mes:=TSearcherMessage(messages.waitForMessage);
      if logging then log('Searcher thread: got message'+strFromPtr(mes));
      if mes=nil then continue;
      if mes.typ in [smtFreeSearcher, smtFreeSearcherAndAccess] then begin
        if logging then log('Searcher thread: message typ smtFree');
        if mes.typ = smtFreeSearcherAndAccess then access.Free;
        mes.free;
        break;
      end;
      performingAction:=mes.typ;
      book:=mes.book;
      if logging then
        if mes.book = nil then log('Message book: nil')
        else begin
          log('Message book: '+strFromPtr(book) +'  ' +book.title+' from '+book.author);
          //for i:=0 to high(book.additional) do log('Book properties: '+book.additional[i].name+' = '+book.additional[i].value);
        end;
      case mes.typ of
        smtNone: ;
        smtConnect: begin
          if logging then log('Searcher thread: message typ smtConnect');
          searcher.connect;
          callNotifyEvent(access.FOnConnected);
        end;
        smtSearch: begin
          if logging then log('Searcher thread: message typ smtSearch');
          if not searcher.Connected then begin //handles timeouts
            if logging then log('Searcher thread: timeout reconnect');
            searcher.connect;
            callNotifyEvent(access.FOnConnected);
          end;
          access.beginResultReading; Searcher.SearchResult.clear; access.endResultReading;
          if Searcher.SearchOptions <> nil then
            debugLastSearchQuery := Searcher.SearchOptions.title + '/'+Searcher.SearchOptions.author+'/'+
                                    Searcher.SearchOptions.getPropertyAdditional('keywords') + '/' + Searcher.SearchOptions.isbn + '/' +
                                    Searcher.SearchOptions.year;// + '@' + inttostr(Searcher.SearchBranch)+':'+inttostr(searcher.HomeBranch);
          searcher.search;
          callPageCompleteEvent(access.FOnSearchPageComplete, true, searcher.SearchNextPageAvailable);
        end;
        smtSearchNext: begin
          if logging then log('Searcher thread: message typ smtSearchNext');
          access.beginResultReading; Searcher.SearchResult.clear; access.endResultReading;
          Searcher.searchNext;
          callPageCompleteEvent(access.FOnSearchPageComplete, false, searcher.SearchNextPageAvailable);
        end;
        smtDetails: begin
          if logging then log('Searcher thread: message typ smtDetails');
          if getproperty('details-searched',book.additional)<>'true' then begin
            if logging then log('Searcher thread: search details for '+book.title+' from '+book.author);
            Searcher.details(book);
            access.beginBookReading;
            addProperty('details-searched','true',book.additional);
            access.endBookReading;
            callBookEvent(access.FOnDetailsComplete,book);
          end;
        end;
        smtOrder: begin
          if logging then log('Searcher thread: message smtOrder: '+book.toSimpleString());
          if (book = nil) or (book.owningAccount = nil) then log('Invalid book')
          else begin
            Searcher.orderSingle(book);
            processPendingMessage(book.owningBook, true);
          end;
          if logging then log('end order');
        end;
        smtCompletePendingMessage: begin
          if logging then log('Searcher thread: message smtCompletePendingMessage: '+book.toSimpleString());
          if book <> nil then Searcher.bookListReader.selectBook(book);
          Searcher.completePendingMessage(mes.pendingMessage, mes.messageResult);
          processPendingMessage(book, (book <> nil) and (book.status in BOOK_CANCELABLE)); //BOOK_CANCELABLE does not mean cancelable, it refers to all ordered books. The idea is that the template will only set the status if the ordering succeeded
          if logging then log('end order');
        end
        else if logging then log('Searcher thread: unknown type');
      end;
      FreeAndNil(mes);
    except
      on e: Exception do begin
        storeException(e,nil,searcher.getLibraryIds,debugLastSearchQuery,searcher.bookListReader.actionTrace.toSharedArray);
        FreeAndNil(mes);
        messages.removeAndFreeAll;
        callNotifyEvent(access.fOnException);
      end;
    end;
  end;
  performingAction:=smtNone;
  FreeAndNil(Searcher);
end;

constructor TSearcherThread.create(template: TMultiPageTemplate; aaccess: TLibrarySearcherAccess);
begin
  InitCriticalSection(fbookAccessSection);
  self.access:=aaccess;
  messages:=TMessageSystem.create;
  performingAction := smtNone;
  changedBook:=nil;
  FreeOnTerminate:=true;
  inherited create(false);
end;

destructor TSearcherThread.destroy;
begin
  if logging then log('TSearcherThread.destroy started');
  try
    while messages.existsMessage do messages.retrieveLatestMessageOrNil.free;
    messages.free;
    DoneCriticalsection(fbookAccessSection);
    {$ifdef android}
    jvmref^^.DetachCurrentThread(jvmref);
    {$endif}
  except
    on e: Exception do
      if logging then log('exception: ' + e.Message);
  end;
  if logging then log('TSearcherThread.destroy ended');
  inherited destroy;
end;

{ TSearcherMessage }

constructor TSearcherMessage.create(t: TSearcherMessageTyp; b: TBook; apendingMessage: TPendingMessage = nil; mesresult: integer = 0 );
begin
  typ:=t;
  book:=b;
  self.pendingMessage := apendingMessage;
  messageResult := mesresult;
end;

end.

