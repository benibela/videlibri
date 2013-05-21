unit androidutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  IniFiles{$ifdef android}, jni, bbjniutils, libraryParser, LCLProc, booklistreader, librarySearcherAccess, androidinternetaccess{$endif};

//procedure deleteLocalRef(jobj: pointer);

var assetPath: string;

procedure beginAssetRead;
function assetFileAsString(name: string): string;
procedure endAssetRead;

function iniFileFromString(data: string): TIniFile;

function getUserConfigPath: string;

function loaded: integer;
procedure uninit;

{$ifdef android}
procedure androidAllThreadsDone();
{$endif}





implementation
uses bbutils, accountlist, applicationconfig, bbdebugtools, libraryAccess;

{$ifndef android}
function assetFileAsString(name: string): string;
begin
  result := strLoadFromFileUTF8(assetPath + name);
end;

function getUserConfigPath: string;
begin
  result := GetAppConfigDir(false);
end;

procedure beginAssetRead;begin end;
procedure endAssetRead;begin end;
procedure uninit;begin end;
function loaded: LongInt; begin end;

{$else}

var assets: jobject = nil;
    assetCount: integer = 0;
    jmAssetManager_Open_StringInputStream, jmInputStream_Read_B: jmethodID;
    videlibriClass: jclass;
    videLibriMethodUserPath, videLibriMethodVLAllThreadsDone: jmethodID;
    accountClass, bookClass: jobject;
    accountClassInit, bookClassInit: jmethodID;
    accountFields: record
      LibIdS, NameS, PassS, PrettyNameS, ExtendDaysI, ExtendZ, HistoryZ: jfieldID;
    end;
    bookFields: record
      authorS, titleS, issueDateGC, dueDateGC, dueDatePrettyS, accountL, historyZ, moreL: jfieldID;
      setPropertyMethod: jmethodID;
    end;
    gregorianCalenderClass: jclass;
    gregorianCalenderClassInit: jmethodID;
    searcherClass: jclass;
    searcherFields: record
      nativePtrJ, totalResultCountI, nextPageAvailableZ: jfieldID;
    end;
    searcherResultInterface: jclass;
    searcherOnSearchFirstPageComplete, searcherOnSearchNextPageComplete, searcherOnSearchDetailsComplete, searcherOnException: jmethodID;

function assetFileAsString(name: string): string;
begin
  beginAssetRead;
  try
    result := j.getAssetAsString(assets, name, jmAssetManager_Open_StringInputStream, jmInputStream_Read_B);
  finally
    endAssetRead;
  end;
end;

procedure beginAssetRead;
begin
  if assetCount = 0 then begin
    assets := needJ.getAssets;
    jmAssetManager_Open_StringInputStream := j.commonMethods_AssetManager_Open_StringInputStream;
    jmInputStream_Read_B := j.commonMethods_InputStream_Read_B;
  end;
  assetCount += 1;
end;

procedure endAssetRead;
begin
  assetCount -= 1;
end;


function getUserConfigPath: string;
begin
  result := j.jStringToStringAndDelete(j.callObjectMethodChecked(jActivityObject, videLibriMethodUserPath));
end;


procedure uninit;
begin
  if jActivityObject <> nil then begin
    j.env^^.DeleteGlobalRef(j.env, jActivityObject);
    jActivityObject := nil;
  end;
end;

procedure androidAllThreadsDone;
begin
  if logging then bbdebugtools.log('androidAllThreadsDone started');
  needJ.callStaticVoidMethod(videlibriClass, videLibriMethodVLAllThreadsDone);
  if logging then bbdebugtools.log('androidAllThreadsDone ended');
end;


procedure Java_de_benibela_VideLibri_Bridge_VLInit(env:PJNIEnv; this:jobject; videlibri: jobject); cdecl;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInit (started)');
  defaultHttpClientClass:=j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/VideLibriHttpClient'));

  videlibriClass :=  j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/VideLibri'));
  jActivityObject := needj.env^^.NewGlobalRef(j.env, videlibri);
  videLibriMethodUserPath := j.getmethod(videlibriClass, 'userPath', '()Ljava/lang/String;');
  videLibriMethodVLAllThreadsDone := j.getstaticmethod(videlibriClass, 'allThreadsDone', '()V');

  accountClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge$Account'));
  accountClassInit := j.getmethod(accountClass, '<init>', '()V');
  with accountFields  do begin
    LibIdS := j.getfield(accountClass, 'libId', 'Ljava/lang/String;');
    NameS := j.getfield(accountClass, 'name', 'Ljava/lang/String;');
    PassS := j.getfield(accountClass, 'pass', 'Ljava/lang/String;');
    PrettyNameS := j.getfield(accountClass, 'prettyName', 'Ljava/lang/String;');
    ExtendDaysI := j.getfield(accountClass, 'extendDays', 'I');
    ExtendZ := j.getfield(accountClass, 'extend', 'Z');
    HistoryZ := j.getfield(accountClass, 'history', 'Z');
  end;

  bookClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge$Book'));
  bookClassInit := j.getmethod(bookClass, '<init>', '()V');
  with bookFields do begin
    authorS := j.getfield(bookClass, 'author', 'Ljava/lang/String;');
    titleS := j.getfield(bookClass, 'title', 'Ljava/lang/String;');
    issueDateGC := j.getfield(bookClass, 'issueDate', 'Ljava/util/GregorianCalendar;');
    dueDateGC := j.getfield(bookClass, 'dueDate', 'Ljava/util/GregorianCalendar;');
    dueDatePrettyS := j.getfield(bookClass, 'dueDatePretty', 'Ljava/lang/String;');
    accountL := j.getfield(bookClass, 'account', 'Lde/benibela/videlibri/Bridge$Account;');
    historyZ := j.getfield(bookClass, 'history', 'Z');
    setPropertyMethod := j.getmethod(bookClass, 'setProperty', '(Ljava/lang/String;Ljava/lang/String;)V');
    moreL := j.getfield(bookClass, 'more', 'Ljava/util/Map;');
  end;


  gregorianCalenderClass := j.newGlobalRefAndDelete(j.getclass('java/util/GregorianCalendar'));
  gregorianCalenderClassInit := j.getmethod(gregorianCalenderClass, '<init>', '(III)V');

  searcherClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge$SearcherAccess'));
  with searcherFields do begin
    nativePtrJ := j.getfield(searcherClass, 'nativePtr', 'J');
    totalResultCountI := j.getfield(searcherClass, 'totalResultCount', 'I');
    nextPageAvailableZ := j.getfield(searcherClass, 'nextPageAvailable', 'Z');
  end;
  searcherOnSearchFirstPageComplete := j.getmethod(searcherClass, 'onSearchFirstPageComplete', '([Lde/benibela/videlibri/Bridge$Book;)V');
  searcherOnSearchNextPageComplete := j.getmethod(searcherClass, 'onSearchNextPageComplete', '([Lde/benibela/videlibri/Bridge$Book;)V');
  searcherOnSearchDetailsComplete := j.getmethod(searcherClass, 'onSearchDetailsComplete', '(Lde/benibela/videlibri/Bridge$Book;)V');
  searcherOnException := j.getmethod(searcherClass, 'onException', '()V');

  beginAssetRead;
  initApplicationConfig;
  endAssetRead;

 // bbdebugtools.OnLog := TStringNotifyEvent(procedureToMethod(TProcedure(@onLog)));

  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInit (ended)');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLFInit(env:PJNIEnv; this:jobject); cdecl;
begin
  finalizeApplicationConfig;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetLibraries(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  lib: TLibrary;
  i: Integer;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraries (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    result := j.newObjectArray(libraryManager.count, j.getclass('java/lang/String'), j.stringToJString(''));
    for i := 0 to libraryManager.count - 1 do begin
      lib := libraryManager[i];
      j.env^^.SetObjectArrayElement(j.env, result, i, j.stringToJString(lib.id+'|'+lib.prettyLocation+'|'+lib.prettyNameLong+'|'+lib.prettyNameShort));
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraries (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLAddAccount(env:PJNIEnv; this:jobject; acc: jobject): jobject; cdecl;
var
  temp: TExtendType;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLAddAccount (started)');
  try
    with accountFields do begin
      if j.getBooleanField(acc, ExtendZ) then temp := etSingleDepends
      else temp := etNever;
      {bbdebugtools.log('AddAccount B');
      bbdebugtools.log('AddAccount B: '+j.getStringField(acc, LibIdS));
      bbdebugtools.log('AddAccount B: '+j.getStringField(acc, PrettyNameS));
      bbdebugtools.log('AddAccount B: '+j.getStringField(acc, NameS));
      bbdebugtools.log('AddAccount B: '+j.getStringField(acc, PassS));}
      accounts.Add(j.getStringField(acc, LibIdS) , j.getStringField(acc, PrettyNameS),
                   j.getStringField(acc, NameS), j.getStringField(acc, PassS),
                   temp, j.getIntField(acc, ExtendDaysI), j.getBooleanField(acc, HistoryZ));
    end;
    accounts.save;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLAddAccount (ended)');
end;

function findAccountIndex(jacc: jobject): integer;
var
  libId: String;
  pretty: String;
  name: String;
  i: Integer;
begin
  with accountFields do begin
    libId := j.getStringField(jacc, LibIdS);
    pretty := j.getStringField(jacc, PrettyNameS);
    name := j.getStringField(jacc, NameS);
  end;

  for i := 0 to accounts.count-1 do
    if (accounts[i].prettyName = pretty) and (accounts[i].getLibrary().id = libId) and (accounts[i].getUser() = name) then
      exit(i);
  exit(-1);
end;

function Java_de_benibela_VideLibri_Bridge_VLDeleteAccount(env:PJNIEnv; this:jobject; acc: jobject): jobject; cdecl;
var
  oldAcc: Integer;
  temp: TCustomAccountAccess;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLDeleteAccount (started)');
  try
    oldAcc := findAccountIndex(acc);
    if oldAcc >= 0 then begin
      temp := accounts[oldAcc];
      accounts.Delete(oldAcc);
      accounts.save;
      temp.remove();
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLDeleteAccount (ended)');
end;


procedure changeAccount(
      i: integer;
      enabled: boolean;
      prettyName, username, password: string;
      keepHistory: boolean;
      extendType: TExtendType; extendDays: integer);
var oldacc: TCustomAccountAccess;
begin
  oldacc := accounts[i];
  oldacc.prettyName:=prettyName;
  oldacc.passWord:=password;
  oldacc.keepHistory:=keepHistory;

  oldacc.extendType:=extendType;
  oldacc.extendDays:=extendDays;

  oldacc.enabled:=enabled;

  if oldacc.getUser() <> username then begin
    oldacc.changeUser(username);
    accounts.Strings[i] := oldacc.getID();
    accounts.save;
  end else
    oldacc.save;
end;

function Java_de_benibela_VideLibri_Bridge_VLChangeAccount(env:PJNIEnv; this:jobject; joldacc, jnewacc: jobject): jobject; cdecl;
var
  oldAccI: Integer;
  temp: TExtendType;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLChangeAccount (started)');
  try
    oldAccI := findAccountIndex(joldacc);
    if oldAccI >= 0 then
      with accountFields do begin
        if j.getBooleanField(jnewacc, ExtendZ) then temp := etSingleDepends
        else temp := etNever;
        changeAccount(oldAccI,
                      true,
                      j.getStringField(jnewacc, PrettyNameS), j.getStringField(jnewacc, NameS), j.getStringField(jnewacc, PassS),
                      j.getBooleanField(jnewacc, HistoryZ),
                      temp, j.getIntField(jnewacc, ExtendDaysI)
                      );
      end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLChangeAccount (ended)');
end;


function Java_de_benibela_VideLibri_Bridge_VLGetAccounts(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  i: Integer;
  temp: jobject;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetAccounts (started)');
  try
    result := j.newObjectArray(accounts.Count, accountClass, nil);
    for i := 0 to accounts.Count - 1 do begin
      temp := j.newObject(accountClass, accountClassInit);
      j.SetObjectArrayElement(result, i,  temp);
      with accountFields  do begin
        j.SetObjectField(temp, LibIdS, j.stringToJString(accounts[i].getLibrary().id));
        j.SetObjectField(temp, NameS, j.stringToJString(accounts[i].getUser()));
        j.SetObjectField(temp, PassS, j.stringToJString(accounts[i].passWord));
        j.SetObjectField(temp, PrettyNameS, j.stringToJString(accounts[i].prettyName));
        j.SetIntField(temp, ExtendDaysI, accounts[i].extendDays);
        j.SetBooleanField(temp, ExtendZ, accounts[i].extendType <> etNever);
        j.SetBooleanField(temp, HistoryZ, accounts[i].keepHistory);
      end;
      j.deleteLocalRef(temp);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetAccounts (ended)');
end;

function getRealAccount(acc: jobject): TCustomAccountAccess;
var
  libId: String;
  user: String;
  i: Integer;
begin
  libId := needj.getStringField(acc, accountFields.LibIdS);
  user := j.getStringField(acc, accountFields.NameS);
  for i := 0 to accounts.Count-1 do
    if (accounts[i].getUser() = user) and (accounts[i].getLibrary().id = libId) then
      exit(accounts[i]);
  result := nil;
  needJ.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Konto nicht gefunden: '+libId+':'+user);
end;


type

{ TJBookSerializer }

TJBookSerializer = object
  book: jobject;
  includeDates: boolean;
  procedure writeProp(n,v:string);
  procedure writeDateProp(n: string; d: integer);
end;


procedure TJBookSerializer.writeProp(n, v: string);
var args: array[0..1] of jvalue;
begin
  case n of
  'title': j.SetStringField(book, bookFields.titleS, v);
  'author': j.SetStringField(book, bookFields.authorS, v);
  else begin
    args[0].l := j.stringToJString(n);
    args[1].l := j.stringToJString(v);
    j.callVoidMethod(book, bookFields.setPropertyMethod, @args);
    j.deleteLocalRef(args[0].l);
    j.deleteLocalRef(args[1].l);
  end;
  end;
end;

procedure TJBookSerializer.writeDateProp(n: string; d: integer);
var args: array[0..2] of jvalue;
    temp: jobject;
    field: jfieldID;
begin
  if not includeDates then exit;
  case n of
  'issueDate': begin field := bookFields.issueDateGC; if d = 0 then exit; end;
  'dueDate': field := bookFields.dueDateGC;
  else exit;
  end;
  dateDecode(d, @args[0].i, @args[1].i, @args[2].i);
  temp := j.newObject(gregorianCalenderClass, gregorianCalenderClassInit, @args);
  j.SetObjectField(book, field, temp);
  j.deleteLocalRef(temp);

  if bookFields.dueDateGC = bookFields.dueDateGC then
    j.SetStringField(book, bookFields.dueDatePrettyS, DateToPrettyStr(d));
end;

function bookToJBook(book: TBook; includeDates: boolean = true; includeAllStrings: boolean = false): jobject;
var temp: TJBookSerializer;
  i: Integer;
begin
  temp.book := j.newObject(bookClass, bookClassInit);
  temp.includeDates := includeDates;
  book.serialize(@temp.writeProp,@temp.writeDateProp);
  if includeAllStrings then
    for i := 0 to high(book.additional) do
      temp.writeProp(book.additional[i].name, book.additional[i].value);
  result := temp.book;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetBooks(env:PJNIEnv; this:jobject; jacc: jobject; jhistory: jboolean): jobject; cdecl;
var
  acc: TCustomAccountAccess;
  history: Boolean;
  books: TBookList;
  i: Integer;
  book: jobject;
begin
  if logging then bbdebugtools.log('Java_de_benibela_VideLibri_Bridge_VLGetBooks started');

  acc := getRealAccount(jacc);
  if acc = nil then exit;
  history := jhistory <> JNI_FALSE;

  if logging then bbdebugtools.log(acc.prettyName);

  system.EnterCriticalSection(updateThreadConfig.libraryAccessSection);


  try
    try
      if history then books := acc.books.old
      else books := acc.books.current;

      result := j.newObjectArray(books.Count, bookClass, nil);
      for i := 0 to books.Count - 1 do begin
        book := bookToJBook(books[i]);
        j.SetBooleanField(book, bookFields.historyZ, history);
        j.SetObjectField(book, bookFields.accountL, jacc);
        j.SetObjectArrayElement(result, i, book);
        j.deleteLocalRef(book);
      end;
    finally
      system.LeaveCriticalsection(updateThreadConfig.libraryAccessSection)
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;

  if logging then bbdebugtools.log('Java_de_benibela_VideLibri_Bridge_VLGetBooks ended');

end;

function Java_de_benibela_VideLibri_Bridge_VLUpdateAccounts(env:PJNIEnv; this:jobject; jacc: jobject; jautoupdate: jboolean; jforceExtend: jboolean): jobject; cdecl;
var
  acc: TCustomAccountAccess;
  autoupdate: boolean;
  forceExtend: boolean;
begin
  bbdebugtools.log('VLUpdateAccounts');
  acc := getRealAccount(jacc);
  if acc = nil then begin bbdebugtools.log('x'); exit;end;

  autoupdate := jautoupdate <> JNI_FALSE;
  forceExtend := jforceExtend <> JNI_FALSE;

  //if ignoreConnErrors and (account.broken = currentDate) then
  //  exit;
  try
 {   EnterCriticalSection(updateThreadConfig.threadManagementSection);
    updateThreadConfig.updateThreadsRunning+=1;
    updateThreadConfig.listUpdateThreadsRunning+=1;
    LeaveCriticalSection(updateThreadConfig.threadManagementSection);
    acc.isThreadRunning:=true;

    updateBooksDirectBlocking(acc, @updateThreadConfig, autoupdate, autoupdate, forceExtend);   }
    updateAccountBookData(acc, autoupdate, autoupdate, forceExtend);
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;

  result := nil;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetPendingExceptions(env:PJNIEnv; this:jobject): jobject;
var
  pendingExceptionClass: jclass;
  pendingExceptionClassInit: jmethodID;
  pendingExceptionFields: record
    accountPrettyNamesS, errorS, detailsS: jfieldID;
  end;


var
  details: String;
  names: String;
  i: Integer;
  temp: jobject;
  k: Integer;
begin
  if logging then bbdebugtools.log('Bridge_VLGetPendingExceptions started');


  pendingExceptionClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge$PendingException'));
  pendingExceptionClassInit := j.getmethod(pendingExceptionClass, '<init>', '()V');
  with pendingExceptionFields do begin
    accountPrettyNamesS := j.getfield(pendingExceptionClass, 'accountPrettyNames', 'Ljava/lang/String;');
    errorS := j.getfield(pendingExceptionClass, 'error', 'Ljava/lang/String;');
    detailsS := j.getfield(pendingExceptionClass, 'details', 'Ljava/lang/String;');
  end;



  result := j.newObjectArray(length(errorMessageList), pendingExceptionClass, nil);
  for i :=  0 to high(errorMessageList) do begin
    temp := j.newObject(pendingExceptionClass, pendingExceptionClassInit);
    with pendingExceptionFields do begin
      details := '';
      names := '';
      for k := 0 to high(errorMessageList[i].details) do begin
        details += errorMessageList[i].details[k].details+LineEnding+LineEnding;
        if names <> '' then names += ', ';
        names += errorMessageList[i].details[k].account.prettyName;
      end;
      j.SetStringField(temp, detailsS, details);
      j.SetStringField(temp, accountPrettyNamesS, names);
      j.SetStringField(temp, errorS, errorMessageList[i].error);
    end;
    j.SetObjectArrayElement(result, i, temp);
    j.deleteLocalRef(temp);
  end;
  if logging then bbdebugtools.log('Bridge_VLGetPendingExceptions ended');
end;

function Java_de_benibela_VideLibri_Bridge_VLGetNotifications(env:PJNIEnv; this:jobject): jobject;
    function grammar(number: integer): string;
    begin
      if number = 1 then exit('1 Buch')
      else exit(IntToStr(number)+' Bücher');
    end;
    function grammar(number: integer; adjective:string): string;
    begin
      if number = 1 then exit('1 '+adjective+'s Buch')
      else exit(IntToStr(number)+' '+adjective+' Bücher');
    end;
var title, text: string;
  booksOverdue: TList;
  booksSoonNotExtendable: TList;
  booksSoon: TList;
  minDateOverdue: integer;
  minDateSoonNotExtendable: integer;
  minDateSoon: integer;
begin
  if logging then bbdebugtools.log('Bridge_VLGetNotifications started');

  title := '';
  text := '';

  findBooksThatMustBeReturned(booksOverdue, booksSoonNotExtendable, booksSoon,
                              minDateOverdue, minDateSoonNotExtendable, minDateSoon);

  if ((lastWarnDate + WarnInterval <= currentDate) and (booksOverdue.Count + booksSoonNotExtendable.Count + booksSoon.Count > 0))
     or (booksOverdue.Count > 0) then begin
    if booksOverdue.Count > 0 then begin
      title := grammar(booksOverdue.Count, 'überfällige')+' ! ';
      text := format('%s seit %s' + LineEnding,
                    [grammar(booksOverdue.Count), DateToPrettyGrammarStr('dem ','',minDateOverdue)]);
    end;
    if (booksSoonNotExtendable.Count > 0) or (booksSoon.Count > 0) then begin
      title += grammar(booksSoonNotExtendable.Count+booksSoon.Count, 'fällige');
      if booksSoonNotExtendable.Count > 0 then begin
        if text <> '' then text += '; ';
        text += format('%s bis %s',
                      [grammar(booksSoonNotExtendable.Count), DateToPrettyGrammarStr('zum ','',minDateSoonNotExtendable)]);
      end;
      if booksSoon.Count > 0 then begin
        if text <> '' then text += '; ';
        text += format('%s bis %s',
                     [grammar(booksSoonNotExtendable.Count, 'verlängerbare'), DateToPrettyGrammarStr('zum ','',minDateSoon)]);
      end;
    end;
  end;

  if title = '' then result := nil
  else begin
    result := j.newObjectArray(2, j.getclass('java/lang/String'), nil);
    j.SetObjectArrayElement(result, 0, j.stringToJString(title));
    j.SetObjectArrayElement(result, 1, j.stringToJString(text));
  end;

  booksSoon.Free;
  booksSoonNotExtendable.free;
  booksOverdue.Free;

  lastWarnDate:=currentDate;
  userConfig.WriteInteger('base','last-warn-date',currentDate);

  if logging then bbdebugtools.log('Bridge_VLGetNotifications ended');
end;


type

{ TLibrarySearcherAccessWrapper }

 TLibrarySearcherAccessWrapper = class(TLibrarySearcherAccess)
  jsearcher: jobject;
  tempBooks: TBookList;
  procedure OnSearchPageCompleteImpl(sender: TObject; firstPage, nextPageAvailable: boolean);
  procedure OnDetailsCompleteImpl(sender: TObject; book: TBook);
  procedure OnExceptionImpl(Sender: TObject);
  constructor create;
  destructor destroy; override;
end;


procedure TLibrarySearcherAccessWrapper.OnSearchPageCompleteImpl(sender: TObject; firstPage, nextPageAvailable: boolean);
var
  books: jobject;
  i: Integer;
  temp: jobject;
begin
  j.SetIntField(jsearcher, searcherFields.totalResultCountI, searcher.SearchResultCount);
  j.SetBooleanField(jsearcher, searcherFields.nextPageAvailableZ, nextPageAvailable);
  books := j.newObjectArray(searcher.SearchResult.Count, bookClass, nil);
  for i := 0 to searcher.SearchResult.Count-1 do begin
    temp := bookToJBook(searcher.SearchResult[i], false, true);
    j.SetObjectArrayElement(books, i, temp);
    j.deleteLocalRef(temp);
  end;
  if firstPage then j.callVoidMethod(jsearcher, searcherOnSearchFirstPageComplete, @books)
  else j.callVoidMethod(jsearcher, searcherOnSearchNextPageComplete, @books);
  j.deleteLocalRef(books);
end;

procedure TLibrarySearcherAccessWrapper.OnDetailsCompleteImpl(sender: TObject; book: TBook);
var
  jbook: jobject;
begin
  jbook := bookToJBook(book, false, true);
  j.callVoidMethod(jsearcher, searcherOnSearchDetailsComplete, @jbook);
  j.deleteLocalRef(jbook);
end;

procedure TLibrarySearcherAccessWrapper.OnExceptionImpl(Sender: TObject);
begin
  j.callVoidMethod(jsearcher, searcherOnException);
end;

constructor TLibrarySearcherAccessWrapper.create;
begin
  tempBooks:=TBookList.Create;
end;

destructor TLibrarySearcherAccessWrapper.destroy;
begin
  tempBooks.free;
  j.deleteGlobalRef(jsearcher);
  inherited destroy;
end;


function unwrapSearcher(const searcher: jobject): TLibrarySearcherAccessWrapper;
begin
  result := TLibrarySearcherAccessWrapper(PtrInt(j.getLongField(searcher, searcherFields.nativePtrJ)));
end;

function wrapSearcherPtr(const searcher: TLibrarySearcherAccess): int64;
begin
  result:=PtrInt(searcher);
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchStart(env:PJNIEnv; this:jobject; searcher: jobject; query: jobject); cdecl;
var
  searcherAccess: TLibrarySearcherAccessWrapper;
  tempAccount: jobject;
  lib: TLibrary;
  more: jobject;
  libId: String;
begin
  with bookFields do begin
    tempAccount := j.getObjectField(query, accountL);
    libId := j.getStringField(tempAccount, accountFields.LibIdS);
    j.deleteLocalRef(tempAccount);

    lib := libraryManager.get(libId);
    if lib = nil then begin
      j.SetLongField(query, searcherFields.nativePtrJ, wrapSearcherPtr(nil));
      exit;
    end;

    searcherAccess := TLibrarySearcherAccessWrapper.create();

    searcherAccess.OnSearchPageComplete:=@searcherAccess.OnSearchPageCompleteImpl;
    searcherAccess.OnDetailsComplete:=@searcherAccess.OnDetailsCompleteImpl;
    searcherAccess.OnException:=@searcherAccess.OnExceptionImpl;

    searcherAccess.newSearch( lib.template );
  //searcherAccess.searcher.clear;
    searcherAccess.searcher.addLibrary(lib);

    searcherAccess.searcher.SearchOptions.author:=j.getStringField(query, authorS);
    searcherAccess.searcher.SearchOptions.title:=j.getStringField(query, titleS);
    more := j.getObjectField(query, moreL);
    searcherAccess.searcher.SearchOptions.year:=j.jStringToStringAndDelete(j.getMapProperty(more, j.stringToJString('year')));
    searcherAccess.searcher.SearchOptions.isbn:=j.jStringToStringAndDelete(j.getMapProperty(more, j.stringToJString('isbn')));
    searcherAccess.searcher.SearchOptions.setProperty('keywords', j.jStringToStringAndDelete(j.getMapProperty(more, j.stringToJString('keywords'))));

    //searcherAccess.searcher.setLocation(searchLocation.Text); that's for digibib
    searcherAccess.connectAsync;
    searcherAccess.searchAsync;

    j.SetLongField(searcher, searcherFields.nativePtrJ, wrapSearcherPtr(searcherAccess));
    searcherAccess.jsearcher:=j.newGlobalRefAndDelete(searcher);
  end;
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchNextPage(env:PJNIEnv; this:jobject; searcher: jobject); cdecl;
var
  sa: TLibrarySearcherAccess;
begin
  sa := unwrapSearcher(searcher);
  sa.searchNextAsync;
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchDetails(env:PJNIEnv; this:jobject; searcher: jobject; jbook: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
  book: TBook;
  more: jobject;
  entrySet: jobject;
  iterator: jobject;
  hasNext: jmethodID;
  next: jmethodID;
  getKey: jmethodID;
  getValue: jmethodID;
  entry: jobject;
begin
  sa := unwrapSearcher(searcher);
  book := TBook.create;
  with bookFields do begin
    book.author := j.getStringField(jbook, authorS);
    book.title := j.getStringField(jbook, titleS);
    more := j.getObjectField(jbook, moreL);


    entrySet := j.callObjectMethodChecked(more, j.getmethod('java/util/Map', 'entrySet', '()Ljava/util/Set;'));
    iterator := j.callObjectMethodChecked(entrySet, j.getmethod('java/util/Set', 'iterator', '()Ljava/util/Iterator;'));

    hasNext := j.getmethod('java/util/Iterator', 'hasNext', '()Z');
    next := j.getmethod('java/util/Iterator', 'next', '()Ljava/lang/Object;');
    getKey := j.getmethod('java/util/Map$Entry', 'getKey', '()Ljava/lang/Object;');
    getValue := j.getmethod('java/util/Map$Entry', 'getValue', '()Ljava/lang/Object;');

    while j.callBooleanMethodChecked(iterator, hasNext) do begin
      entry := j.callObjectMethodChecked(iterator, next);
      book.setProperty(j.jStringToStringAndDelete(j.callObjectMethodChecked(entry, getKey)),
                       j.jStringToStringAndDelete(j.callObjectMethodChecked(entry, getValue)));
      j.deleteLocalRef(entry);
    end;
  end;
  sa.tempBooks.add(book);
  sa.detailsAsyncSave(book);
end;



procedure Java_de_benibela_VideLibri_Bridge_VLSearchEnd(env:PJNIEnv; this:jobject; searcher: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
begin
  sa := unwrapSearcher(searcher);
  sa.free;
  j.SetLongField(searcher, searcherFields.nativePtrJ, 0);
end;

type TJOptionClass = record
  c: jclass;
  init: jmethodID;
  nearTimeI, loggingZ: jfieldID;
end;

function getOptionClass: TJOptionClass;
begin
  with result do begin
    c := j.getclass('de/benibela/videlibri/Bridge$Options');
    init := j.getmethod(c, '<init>', '()V');
    nearTimeI := j.getfield(c, 'nearTime', 'I');
    loggingZ := j.getfield(c, 'logging', 'Z');
  end;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetOptions(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  lib: TLibrary;
  i: Integer;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptions (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    with getOptionClass do begin
      result := j.newObject(c, init);
      j.SetIntField(result, nearTimeI, userConfig.ReadInteger('base','near-time',3));
      j.SetBooleanField(result, loggingZ, logging);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptions (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLSetOptions(env:PJNIEnv; this:jobject; options: jobject): jobject; cdecl;
var
  lib: TLibrary;
  i: Integer;
  optionClass: jclass;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptions (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    with getOptionClass do begin
      userConfig.WriteInteger('base','near-time', j.getIntField(options, nearTimeI));
      redTime := currentDate + j.getIntField(options, nearTimeI);
      logging := j.getBooleanField(options, loggingZ);
      userConfig.WriteBool('base','logging', logging);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptions (ended)');
end;








const nativeMethods: array[1..17] of JNINativeMethod=
  ((name:'VLInit';          signature:'(Lde/benibela/videlibri/VideLibri;)V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLInit)
   ,(name:'VLFinalize';      signature:'()V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLFInit)

   ,(name:'VLGetLibraries'; signature:'()[Ljava/lang/String;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetLibraries)

   ,(name:'VLAddAccount'; signature:'(Lde/benibela/videlibri/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLAddAccount)
   ,(name:'VLDeleteAccount'; signature:'(Lde/benibela/videlibri/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLDeleteAccount)
   ,(name:'VLChangeAccount'; signature:'(Lde/benibela/videlibri/Bridge$Account;Lde/benibela/videlibri/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLChangeAccount)
   ,(name:'VLGetAccounts'; signature:'()[Lde/benibela/videlibri/Bridge$Account;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetAccounts)
   ,(name:'VLGetBooks'; signature:'(Lde/benibela/videlibri/Bridge$Account;Z)[Lde/benibela/videlibri/Bridge$Book;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetBooks)

   ,(name:'VLUpdateAccount'; signature:'(Lde/benibela/videlibri/Bridge$Account;ZZ)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLUpdateAccounts)
   ,(name:'VLTakePendingExceptions'; signature: '()[Lde/benibela/videlibri/Bridge$PendingException;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetPendingExceptions)

   ,(name:'VLGetNotifications'; signature: '()[Ljava/lang/String;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetNotifications)

   ,(name:'VLSearchStart'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;Lde/benibela/videlibri/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchStart)
   ,(name:'VLSearchNextPage'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchNextPage)
   ,(name:'VLSearchDetails'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;Lde/benibela/videlibri/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchDetails)
   ,(name:'VLSearchEnd'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchEnd)

   ,(name:'VLSetOptions'; signature: '(Lde/benibela/videlibri/Bridge$Options;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSetOptions)
   ,(name:'VLGetOptions'; signature: '()Lde/benibela/videlibri/Bridge$Options;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetOptions)
  );




function loaded: integer;
var bridgeClass: jclass;
begin
  needJ;

  bridgeClass := j.env^^.FindClass(j.env, 'de/benibela/videlibri/Bridge');
  if (not assigned(bridgeClass)) or (j.env^^.ExceptionCheck(j.env)<>0) then begin
    bbdebugtools.log('failed to find VideLibri Bridge');
    exit(JNI_ERR);
  end;
  if j.env^^.RegisterNatives(j.env, bridgeClass, @NativeMethods[low(nativeMethods)],length(NativeMethods)) < 0 then begin
    bbdebugtools.log('failed to register methods');
    exit(JNI_ERR);
  end;
  result := JNI_VERSION_1_4;
end;

{$endif}


function iniFileFromString(data: string): TIniFile;
var
  stream: TStringStream;
begin
  stream := TStringStream.Create(data);
  result := TIniFile.Create(stream);
  stream.free;
end;




end.

