unit librarySearcherAccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils , librarySearcher,booklistreader,bbutils,simplexmlparser;

type

TBookNotifyEvent=procedure (sender: TObject; book: TBook) of object;
TLibrarySearcherAccess = class;

{ TLibrarySearcherAccess }

TSearcherMessageTyp=(smtFree, smtConnect, smtSearch, smtDetails, smtImage, smtDisconnect);

{ TSearcherMessage }

TSearcherMessage = class
  typ: TSearcherMessageTyp;
  book: tbook;
  constructor create(t:TSearcherMessageTyp; b:TBook=nil);
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
  changedBook: tbook;
  
  performingAction: boolean;
  
  procedure callNotifyEventSynchronized;
  procedure callBookEventSynchronized;
  procedure callNotifyEvent(event: TNotifyEvent);
  procedure callBookEvent(event: TBookNotifyEvent; book: tbook);

  procedure execute;override;
public
  constructor create(template: TBookListTemplate; aaccess: TLibrarySearcherAccess);
  destructor destroy;override;
end;


TLibrarySearcherAccess = class
private
  FOnException: TNotifyEvent;
  fthread: TSearcherThread;
  ftemplate: TBookListTemplate;
  FOnConnected: TNotifyEvent;
  FOnDetailsComplete: TBookNotifyEvent;
  FOnImageComplete: TBookNotifyEvent;
  FOnSearchComplete: TNotifyEvent;

  function operationActive: boolean;
  procedure removeOldMessageOf(typ: TSearcherMessageTyp);
  function GetSearcher: TLibrarySearcher;
  procedure threadException();
public
  constructor create(template: TBookListTemplate);
  destructor destroy; override;

  procedure beginBookReading;
  procedure endBookReading;
  procedure beginResultReading;
  procedure endResultReading;


  procedure newSearch; //ensures that all operations are finished

  procedure connectAsync;
  procedure searchAsync;
  procedure detailsAsyncSave(book: TBook); //save means they make sure the
  procedure imageAsyncSave(book: TBook);   //book is not updated only once
  procedure disconnectAsync;

  property OnException: TNotifyEvent read FOnException write FOnException;
  property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
  property OnSearchComplete: TNotifyEvent read FOnSearchComplete write FOnSearchComplete;
  property OnDetailsComplete: TBookNotifyEvent read FOnDetailsComplete write FOnDetailsComplete;
  property OnImageComplete: TBookNotifyEvent read FOnImageComplete write FOnImageComplete;
  property searcher: TLibrarySearcher read GetSearcher;
end;
  
implementation

uses applicationconfig, bbdebugtools;



{ TLibrarySearcherAccess }

function TLibrarySearcherAccess.GetSearcher: TLibrarySearcher;
begin
  Result:=fthread.Searcher;
end;

procedure TLibrarySearcherAccess.threadException();
begin
  if logging then log('TLibrarySearcherAccess.threadException called');
  if assigned(OnException) then
    OnException(self);
end;

constructor TLibrarySearcherAccess.create(template: TBookListTemplate);
begin
  ftemplate:=template;
  fthread:=TSearcherThread.Create(template,self);
end;

destructor TLibrarySearcherAccess.destroy;
begin
  fthread.messages.storeMessage(TSearcherMessage.Create(smtFree));
  inherited destroy;
end;

procedure TLibrarySearcherAccess.beginBookReading;
begin
  EnterCriticalsection(fthread. fbookAccessSection);
end;

procedure TLibrarySearcherAccess.endBookReading;
begin
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
  result:=fthread.messages.existsMessage or fthread.performingAction;
end;

procedure TLibrarySearcherAccess.removeOldMessageOf(typ: TSearcherMessageTyp);
var list: TFPList;
    i:longint;
begin
  list:=fthread.messages.openDirectMessageAccess;
  for i:=list.Count-1 downto 0 do
    if TSearcherMessage(list[i]).typ=typ then
      list.Delete(i);
  fthread.messages.closeDirectMessageAccess(list);
end;

procedure TLibrarySearcherAccess.newSearch;
begin
  if operationActive then begin
    fthread.messages.storeMessage(TSearcherMessage.Create(smtFree));
    fthread:=TSearcherThread.Create(ftemplate,self);
  end;
end;

procedure TLibrarySearcherAccess.connectAsync;
begin
  removeOldMessageOf(smtConnect);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtConnect));
end;

procedure TLibrarySearcherAccess.searchAsync;
begin
  removeOldMessageOf(smtSearch);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtSearch));
end;

procedure TLibrarySearcherAccess.detailsAsyncSave(book: TBook);
begin
  removeOldMessageOf(smtDetails);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtDetails,book));
end;

procedure TLibrarySearcherAccess.imageAsyncSave(book: TBook);
begin
  removeOldMessageOf(smtImage);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtImage,book));
end;

procedure TLibrarySearcherAccess.disconnectAsync;
begin
  removeOldMessageOf(smtDisconnect);
  fthread.messages.storeMessage(TSearcherMessage.Create(smtDisconnect));
end;

{ TSearcherThread }

procedure TSearcherThread.callNotifyEventSynchronized;
begin
  if self<>access.fthread then exit;
  notifyEvent(self);
end;

procedure TSearcherThread.callBookEventSynchronized;
begin
  if self<>access.fthread then exit;
  bookNotifyEvent(self,changedBook);
end;

procedure TSearcherThread.callNotifyEvent(event: TNotifyEvent);
begin
  if event=nil then exit;
  notifyEvent:=event;
  Synchronize(@callNotifyEventSynchronized);
end;

procedure TSearcherThread.callBookEvent(event: TBookNotifyEvent; book: tbook);
begin
  if event=nil then exit;
  bookNotifyEvent:=event;
  changedBook:=book;
  Synchronize(@callBookEventSynchronized);
end;

procedure TSearcherThread.execute;
var mes: TSearcherMessage;
    image:string;
    book: tbook;
begin
  while true do begin
    try
      performingAction:=false;
      while not messages.existsMessage do sleep(5);
      mes:=TSearcherMessage(messages.retrieveMessageOrNil);
      if mes=nil then continue;
      if mes.typ=smtFree then begin
        mes.free;
        exit;
      end;
      performingAction:=true;
      book:=mes.book;
      case mes.typ of
        smtConnect: begin
          searcher.connect;
          callNotifyEvent(access.FOnConnected);
        end;
        smtSearch: begin
          searcher.search;
          callNotifyEvent(access.FOnSearchComplete);
        end;
        smtDetails:
          if getproperty('details-searched',book.additional)<>'true' then begin
            Searcher.details(book);
            access.beginBookReading;
            addProperty('details-searched','true',book.additional);
            access.endBookReading;
            callBookEvent(access.FOnDetailsComplete,book);
          end;
        smtImage:
          if (getproperty('image-searched',book.additional)<>'true') and (getProperty('image-url',book.additional)<>'') then begin
            image:=searcher.bookListReader.internet.get(getProperty('image-url',book.additional));
            access.beginBookReading;
            addProperty('image',image,book.additional);
            addProperty('image-searched','true',book.additional);
            access.endBookReading;
            callBookEvent(access.FOnImageComplete,book);
          end;
      end;
      FreeAndNil(mes);
    except
      on e: Exception do begin
        storeException(e,nil);
        FreeAndNil(mes);
        messages.removeAndFreeAll;
        Synchronize(@access.threadException);
      end;
    end;
  end;
end;

constructor TSearcherThread.create(template: TBookListTemplate; aaccess: TLibrarySearcherAccess);
begin
  InitCriticalSection(fbookAccessSection);
  Searcher:=TLibrarySearcher.create(template);
  Searcher.bookListReader.bookAccessSection:=@fbookAccessSection;
  self.access:=aaccess;
  messages:=TMessageSystem.create;
  FreeOnTerminate:=true;
  inherited create(false);
end;

destructor TSearcherThread.destroy;
begin
  FreeAndNil(Searcher);
  while messages.existsMessage do messages.retrieveLatestMessageOrNil.free;
  messages.free;
  DoneCriticalsection(fbookAccessSection);
  inherited destroy;
end;

{ TSearcherMessage }

constructor TSearcherMessage.create(t: TSearcherMessageTyp; b: TBook);
begin
  typ:=t;
  book:=b;
end;

end.

