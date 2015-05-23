unit librarySearcherAccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils , librarySearcher,libraryParser, booklistreader,bbutils,messagesystem,simplexmlparser,multipagetemplate,forms;

type

TBookNotifyEvent=procedure (sender: TObject; book: TBook) of object;
TPageCompleteNotifyEvent=procedure (sender: TObject; firstPage, nextPageAvailable: boolean) of object;
TPendingMessageEvent=procedure (sender: TObject; book: TBook; pendingMessage: TPendingMessage) of object;
TLibrarySearcherAccess = class;

{ TLibrarySearcherAccess }

TSearcherMessageTyp=(smtFree, smtConnect, smtSearch, smtSearchNext, smtDetails, smtImage, smtOrder, smtOrderConfirmed, smtCompletePendingMessage, smtDisconnect);

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
  connectedEvent: TNotifyEvent;
  changedBook: tbook;
  firstPageForSync, nextPageAvailableForSync: boolean;
  pendingMessage: TPendingMessage;

  performingAction: boolean;

  procedure desktopSynchronized(t: TThreadMethod);

  procedure callNotifyEventSynchronized;
  procedure callBookEventSynchronized;
  procedure callPageCompleteEventSynchronized;
  procedure callPendingMessageEventSynchronized;

  procedure callNotifyEvent(event: TNotifyEvent);
  procedure callBookEvent(event: TBookNotifyEvent; book: tbook);
  procedure callPageCompleteEvent(event: TPageCompleteNotifyEvent; firstPage, nextPageAvailable: boolean);
  procedure callPendingMessageEvent(event: TPendingMessageEvent; book: TBook; pendingMes: TPendingMessage);

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
  FOnDetailsComplete, FOnOrderComplete, FOnOrderConfirm: TBookNotifyEvent;
  FOnImageComplete: TBookNotifyEvent;
  FOnSearchPageComplete: TPageCompleteNotifyEvent;
  FOnTakePendingMessage: TPendingMessageEvent;

  procedure removeOldMessageOf(typ: TSearcherMessageTyp);
  function GetSearcher: TLibrarySearcher;
  procedure threadException();
public
  function operationActive: boolean;

  constructor create();
  destructor destroy; override;

  procedure beginBookReading;
  procedure endBookReading;
  procedure beginResultReading;
  procedure endResultReading;


  procedure newSearch(template: TMultiPageTemplate); //ensures that all operations are finished
  procedure prepareNewSearchWithoutDisconnect; //ensures that all operations are finished, without changing the connection state

  procedure connectAsync;
  procedure searchAsync;
  procedure searchNextAsync;
  procedure detailsAsyncSave(book: TBook); //save means they make sure the
  procedure imageAsyncSave(book: TBook);   //book is not updated only once
  procedure orderAsync(account: TCustomAccountAccess; book: TBook);
  procedure orderConfirmedAsync(book: TBook);
  procedure completePendingMessage(book: TBook; pm: TPendingMessage; result: integer);
  procedure disconnectAsync;

  property OnException: TNotifyEvent read FOnException write FOnException;
  property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
  property OnSearchPageComplete: TPageCompleteNotifyEvent read FOnSearchPageComplete write FOnSearchPageComplete;
  property OnDetailsComplete: TBookNotifyEvent read FOnDetailsComplete write FOnDetailsComplete;
  property OnOrderComplete: TBookNotifyEvent read FOnOrderComplete write FOnOrderComplete;
  property OnOrderConfirm: TBookNotifyEvent read FOnOrderConfirm write FOnOrderConfirm;
  property OnTakePendingMessage: TPendingMessageEvent read FOnTakePendingMessage write FOnTakePendingMessage;
  property OnPendingMessageCompleted: TNotifyEvent read FOnPendingMessageCompleted write FOnPendingMessageCompleted;
  property OnImageComplete: TBookNotifyEvent read FOnImageComplete write FOnImageComplete;
  property searcher: TLibrarySearcher read GetSearcher;
end;

type

  { TSearchTarget }

  TSearchTarget = class
    name: string;
    lib: TLibrary; //might be nil
    template: TMultiPageTemplate;
    constructor create(aname: string; alib: TLibrary; atemplate: TMultiPageTemplate);
  end;

  { TSearchableLocations }

  TSearchableLocations = record
    locations:  TStringList; //string->stringlist of TSearchTarget
    searchTemplates: TStringList;
  end;

function getSearchableLocations: TSearchableLocations;
implementation

uses applicationconfig, bbdebugtools, androidutils {$ifdef android}, bbjniutils{$endif};

function getSearchableLocations: TSearchableLocations;
var digibib: TMultiPageTemplate;
  temp: TStringList;
  i: Integer;
  loc: String;
begin
  with result do begin
    locations := TStringList.Create;
    locations.OwnsObjects := true;

    searchTemplates := TStringList.Create;
    searchTemplates.OwnsObjects := true;
    searchTemplates.text := assetFileAsString('libraries/search/search.list');
    for i := searchTemplates.count-1 downto 0 do begin
      searchTemplates[i] := trim(searchTemplates[i]);
      if searchTemplates[i] = '' then searchTemplates.Delete(i);
    end;
    for i :=0 to searchTemplates.count-1 do begin
      searchTemplates.Objects[i] := TMultiPageTemplate.create();
      TMultiPageTemplate(searchTemplates.Objects[i]).loadTemplateWithCallback(@assetFileAsString, StringReplace('libraries\search\templates\' +trim(searchTemplates[i])+'\','\',DirectorySeparator,[rfReplaceAll]),trim(searchTemplates[i]));
      if searchTemplates[i] <> 'digibib' then begin
        temp := TStringList.Create;
        temp.OwnsObjects := true;
        temp.AddObject(searchTemplates[i], TSearchTarget.create(searchTemplates[i], nil, TMultiPageTemplate(searchTemplates.Objects[i])));
        locations.AddObject(searchTemplates[i], temp);
      end else digibib := TMultiPageTemplate(searchTemplates.Objects[i]);
    end;

    if digibib = nil then raise ELibraryException.create('Digibib template not found');

    for i := 0 to libraryManager.getLibraryCountInEnumeration-1 do begin
      loc := libraryManager[i].prettyLocation;
      if libraryManager[i].template.findAction('search') <> nil then begin
        if locations.IndexOf(loc) < 0 then
          locations.AddObject(loc, TStringList.Create);
        temp := TStringList(locations.Objects[locations.IndexOf(loc)]);
        temp.OwnsObjects := true;
        temp.AddObject(libraryManager[i].prettyNameLong, TSearchTarget.create(libraryManager[i].prettyNameLong, libraryManager[i], libraryManager[i].template));
      end;
      if libraryManager[i].variables.IndexOfName('searchid-digibib') >= 0 then begin
        if locations.IndexOf(loc+' (digibib)') < 0 then
          locations.AddObject(loc+' (digibib)', TStringList.Create);
        temp := TStringList(locations.Objects[locations.IndexOf(loc+' (digibib)')]);
        temp.OwnsObjects := true;
        temp.AddObject(libraryManager[i].prettyNameLong, TSearchTarget.create(libraryManager[i].prettyNameLong, libraryManager[i], digibib));
      end;
    end;

    locations.Sort;
  end;
end;

{ TSearchTarget }

constructor TSearchTarget.create(aname: string; alib: TLibrary; atemplate: TMultiPageTemplate);
begin
  name := aname;
  lib := alib;
  template := atemplate;
end;

{ TLibrarySearcherAccess }

function TLibrarySearcherAccess.GetSearcher: TLibrarySearcher;
begin
  if not assigned(fthread) then exit(nil);
  Result:=fthread.Searcher;
end;

procedure TLibrarySearcherAccess.threadException();
begin
  if logging then log('TLibrarySearcherAccess.threadException called');
  if assigned(OnException) then
    OnException(self);
end;

constructor TLibrarySearcherAccess.create();
begin
end;

destructor TLibrarySearcherAccess.destroy;
begin
  if assigned(fthread) then fthread.messages.storeMessage(TSearcherMessage.Create(smtFree));
  inherited destroy;
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
  result:=assigned(fthread) and (fthread.messages.existsMessage or fthread.performingAction);
end;

procedure TLibrarySearcherAccess.removeOldMessageOf(typ: TSearcherMessageTyp);
var list: TFPList;
    i:longint;
begin
  if not assigned(fthread) then exit;
  list:=fthread.messages.openDirectMessageAccess;
  for i:=list.Count-1 downto 0 do
    if TSearcherMessage(list[i]).typ=typ then
      list.Delete(i);
  fthread.messages.closeDirectMessageAccess(list);
end;

procedure TLibrarySearcherAccess.newSearch(template: TMultiPageTemplate);
begin
  if (operationActive) or not Assigned(fthread) or (ftemplate <> template) then begin
    if assigned(fthread) then fthread.messages.storeMessage(TSearcherMessage.Create(smtFree));
    ftemplate := template;
    fthread:=TSearcherThread.Create(ftemplate,self);
  end;
end;

procedure TLibrarySearcherAccess.prepareNewSearchWithoutDisconnect;
var
  list: TFPList;
  i: Integer;
begin
  list:=fthread.messages.openDirectMessageAccess;
  for i:=list.Count-1 downto 0 do
    if not (TSearcherMessage(list[i]).typ in [smtFree, smtConnect, smtDisconnect]) then
      list.Delete(i);
  fthread.messages.closeDirectMessageAccess(list);

  while operationActive do begin
    sleep(50);
    {$ifndef android}application.ProcessMessages;{$endif}
  end;
end;

procedure TLibrarySearcherAccess.connectAsync;
begin
  if not assigned(fthread) then exit;
  removeOldMessageOf(smtConnect);
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

procedure TLibrarySearcherAccess.imageAsyncSave(book: TBook);
begin
  if not assigned(fthread) then exit;
  removeOldMessageOf(smtImage);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtImage,book));
end;

procedure TLibrarySearcherAccess.orderAsync(account: TCustomAccountAccess; book: TBook);
begin
  if not assigned(fthread) then exit;
  book.owner := account;
  fthread.messages.storeMessage(TSearcherMessage.Create(smtOrder,book));
end;

procedure TLibrarySearcherAccess.orderConfirmedAsync(book: TBook);
begin
  if not assigned(fthread) then exit;
  fthread.messages.storeMessage(TSearcherMessage.Create(smtOrderConfirmed,book));
end;

procedure TLibrarySearcherAccess.completePendingMessage(book: TBook; pm: TPendingMessage; result: integer);
begin
  if not assigned(fthread) then exit;
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


procedure TSearcherThread.execute;
var mes: TSearcherMessage;
    image:string;
    book: tbook;
    i: Integer;
    debugLastSearchQuery: String;
begin
  while true do begin
    try
      performingAction:=false;
      if logging then log('Searcher thread: wait for message');
      while not messages.existsMessage do sleep(5);
      if logging then log('Searcher thread: possible message');
      mes:=TSearcherMessage(messages.retrieveMessageOrNil);
      if logging then log('Searcher thread: got message'+strFromPtr(mes));
      if mes=nil then continue;
      if mes.typ=smtFree then begin
        if logging then log('Searcher thread: message typ smtFree');
        mes.free;
        break;
      end;
      performingAction:=true;
      book:=mes.book;
      if logging then
        if mes.book = nil then log('Message book: nil')
        else begin
          log('Message book: '+strFromPtr(book) +'  ' +book.title+' from '+book.author);
          //for i:=0 to high(book.additional) do log('Book properties: '+book.additional[i].name+' = '+book.additional[i].value);
        end;
      case mes.typ of
        smtConnect: begin
          if logging then log('Searcher thread: message typ smtConnect');
          access.beginResultReading;
          searcher.connect;
          access.endResultReading;
          callNotifyEvent(access.FOnConnected);
        end;
        smtSearch: begin
          if logging then log('Searcher thread: message typ smtSearch');
          if not searcher.Connected then begin //handles timeouts
            if logging then log('Searcher thread: timeout reconnect');
            access.beginResultReading;
            searcher.connect;
            access.endResultReading;
            callNotifyEvent(access.FOnConnected);
          end;
          access.beginResultReading; Searcher.SearchResult.clear; access.endResultReading;
          if Searcher.SearchOptions <> nil then
            debugLastSearchQuery := Searcher.SearchOptions.title + '/'+Searcher.SearchOptions.author+'/'+
                                    Searcher.SearchOptions.getPropertyAdditional('keywords') + '/' + Searcher.SearchOptions.isbn + '/' +
                                    Searcher.SearchOptions.year + '@' + inttostr(Searcher.SearchBranch)+':'+inttostr(searcher.HomeBranch);
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
        smtImage: begin
          if logging then log('Searcher thread: message typ smtImage: image-searched: '+getproperty('image-searched',book.additional)+'  image-url: '+getProperty('image-url',book.additional));
          if (getproperty('image-searched',book.additional)<>'true') then begin
            if (getProperty('image-url',book.additional)<>'') then begin
              if logging then log('image url: '+getProperty('image-url',book.additional));
              image:=searcher.bookListReader.internet.get(getProperty('image-url',book.additional));
            end else image:='';
            if (image = '') and (book.isbn <> '') then
              image:=searcher.bookListReader.internet.get('http://covers.openlibrary.org/b/isbn/'+book.getNormalizedISBN+'-M.jpg');
            if logging then log('got image: size: '+IntToStr(length(image)));
            access.beginBookReading;
            addProperty('image',image,book.additional);
            addProperty('image-content-type', searcher.bookListReader.internet.getLastContentType,book.additional);
            addProperty('image-searched','true',book.additional);
            access.endBookReading;
            callBookEvent(access.FOnImageComplete,book);
            if logging then log('end image');
          end;
        end;
        smtOrder, smtOrderConfirmed: begin
          if logging then log('Searcher thread: message smtOrder: '+book.toSimpleString());
          if (book = nil) or (book.owner = nil) then log('Invalid book')
          else begin
            if (mes.typ = smtOrderConfirmed) or not searcher.orderNeedsConfirmation(book) then begin
              Searcher.orderSingle(book);
              if Searcher.bookListReader.pendingMessage <> nil then begin
                callPendingMessageEvent(access.FOnTakePendingMessage, book, Searcher.bookListReader.pendingMessage);
                Searcher.bookListReader.pendingMessage := nil;
               end else
                callBookEvent(access.FOnOrderComplete, book);
            end else begin
              Searcher.orderConfirmSingle(book);
              callBookEvent(access.FOnOrderConfirm, book);
            end;
          end;
          if logging then log('end order');
        end;
        smtCompletePendingMessage: begin
          if logging then log('Searcher thread: message smtCompletePendingMessage: '+book.toSimpleString());
          if book <> nil then Searcher.bookListReader.selectBook(book);
          Searcher.completePendingMessage(mes.pendingMessage, mes.messageResult);
          if Searcher.bookListReader.pendingMessage <> nil then begin
            callPendingMessageEvent(access.FOnTakePendingMessage, book, Searcher.bookListReader.pendingMessage);
            Searcher.bookListReader.pendingMessage := nil;
          end else if book.status = bsOrdered then begin
            callBookEvent(access.FOnOrderComplete, book);
          end else callNotifyEvent(access.FOnPendingMessageCompleted);
          if logging then log('end order');
        end
        else if logging then log('Searcher thread: unknown type');
      end;
      FreeAndNil(mes);
    except
      on e: Exception do begin
        storeException(e,nil,searcher.getLibraryIds,debugLastSearchQuery);
        FreeAndNil(mes);
        messages.removeAndFreeAll;
        desktopSynchronized(@access.threadException);
      end;
    end;
  end;
  performingAction:=false;
end;

constructor TSearcherThread.create(template: TMultiPageTemplate; aaccess: TLibrarySearcherAccess);
begin
  InitCriticalSection(fbookAccessSection);
  Searcher:=TLibrarySearcher.create(template);
  Searcher.bookListReader.bookAccessSection:=@fbookAccessSection;
  self.access:=aaccess;
  messages:=TMessageSystem.create;
  changedBook:=nil;
  FreeOnTerminate:=true;
  inherited create(false);
end;

destructor TSearcherThread.destroy;
begin
  FreeAndNil(Searcher);
  while messages.existsMessage do messages.retrieveLatestMessageOrNil.free;
  messages.free;
  DoneCriticalsection(fbookAccessSection);
  {$ifdef android}
  jvmref^^.DetachCurrentThread(jvmref);
  {$endif}
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

