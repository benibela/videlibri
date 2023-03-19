unit coverLoaderAccess;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
interface

uses
  Classes, SysUtils, booklistreader, vlmaps, messagesystem, internetaccess;

type
TCoverRequestData = record
  isbn: string;
  imageUrl: string;
  function id: string;
end;

TCoverImageInfo = class
  request: TCoverRequestData;
  image: string;
  imageExtension: string;
end;

TCoverMessageTyp=(cmtFree, cmtRequest);
TCoverThreadMessage = class
  typ: TCoverMessageTyp;
  request: TCoverRequestData;
end;

TCoverThread = class(TThread)
  //ccessSection: TRTLCriticalSection;
  messages: TMessageSystem;
  internet: tinternetaccess;

  outputAccessSection: TRTLCriticalSection;
  outputImageInfo: array of TCoverImageInfo;

  OnImageReceived: TThreadMethod;
  constructor Create();
  procedure Execute; override;
  destructor Destroy; override;
end;

TCoverLoaderAccess = class
public
  cache: TMapStringOwningObject;
  thread: TCoverThread;
  emptyImageInfo: TCoverImageInfo;
  OnImageReceived: TNotifyEvent;
  //returns nil if not in cache
  function getImageInfo(lockedBook: TBook): TCoverImageInfo;
  function getImageInfo(isbn, imageUrl: string): TCoverImageInfo;
  constructor create;
  procedure shutdownAndFree;
  destructor Destroy; override;
protected
  procedure removeOldMessages;
  procedure threadReceivedImage;
  procedure threadTerminated(t: tobject);
end;

implementation

uses bbdebugtools,bbutils,bbutilsbeta;

constructor TCoverThread.Create();
begin
  inherited Create(false);
  messages := TMessageSystem.create;
  internet := defaultInternetAccessClass.create(); //todo: set accept-language?
end;

procedure TCoverThread.Execute;
  procedure doRequest(const request: TCoverRequestData);
  var imageInfo: TCoverImageInfo;
    temp, imageUrl, contentType: String;
    images, covers: TStringArray;
  begin
    if logging then log('Cover thread: message typ cmtImage '+request.isbn);
    imageInfo := TCoverImageInfo.Create;
    imageInfo.request := request;
    images := strSplit(request.imageUrl, LineEnding);
    if request.isbn <> '' then begin
      covers := TBook.getCoverURLS(request.isbn, 1000, 1000);
      images := Concat(images, covers);
    end;

    if logging then log('image candidate urls: '+strJoin(images, LineEnding));
    for temp in images do begin
      imageUrl := strTrimAndNormalize(temp);
      if imageUrl = '' then continue;
      try
        imageinfo.image:=internet.get(imageUrl);

        contentType := internet.getLastContentType;
        if striContains(contentType, 'image/jpeg') or striContains(contentType, 'image/jpg') then imageInfo.imageExtension := '.jpg'
        else if striContains(contentType, 'image/png') then imageInfo.imageExtension := '.png'
        else if striContains(contentType, 'image/gif') then imageInfo.imageExtension := '.gif'
        else imageInfo.imageExtension := ExtractFileExt(imageUrl);

      except
        on e: EInternetException do imageinfo.image := '';
      end;
      if imageinfo.image <> '' then break;
    end;

    outputAccessSection.enter;
    SetLength(outputImageInfo, length(outputImageInfo) + 1);
    outputImageInfo[high(outputImageInfo)] := imageInfo;
    outputAccessSection.leave;

    if assigned(OnImageReceived) then
      Synchronize(OnImageReceived);
    if logging then log('end image');
  end;

var
  mes: TCoverThreadMessage;
begin
  while true do begin
    try
      if logging then log('Cover thread: wait for message');
      mes:=TCoverThreadMessage(messages.waitForMessage);
      if logging then log('Cover thread: got message'+strFromPtr(mes));
      if mes=nil then continue;
      case mes.typ of
        cmtFree: begin
          if logging then log('Cover thread: message typ cmtFree');
          mes.free;
          break;
        end;
        cmtRequest: doRequest(mes.request);
      end;
      FreeAndNil(mes);
    except
      on e: Exception do begin
        FreeAndNil(mes);
        messages.removeAndFreeAll;
      end;
    end;
  end;
end;

destructor TCoverThread.Destroy;
begin
  internet.free;
  messages.Free;
  inherited Destroy;
end;

function TCoverRequestData.id: string;
begin
  result := isbn + #0 + imageUrl;
end;

function TCoverLoaderAccess.getImageInfo(lockedBook: TBook): TCoverImageInfo;
begin
  result := getImageInfo(lockedBook.isbn, lockedBook.getProperty('image-url'));
end;

function TCoverLoaderAccess.getImageInfo(isbn, imageUrl: string): TCoverImageInfo;
var request: TCoverRequestData;
  i: Integer;
  msg: TCoverThreadMessage;
begin
  request.isbn := trim(isbn);
  request.imageUrl := trim(imageUrl);
  if (request.isbn = '') and (request.imageUrl = '') then exit(emptyImageInfo);
  i := cache.IndexOf(request.id);
  if i >= 0 then exit(cache.Objects[i] as TCoverImageInfo);
  removeOldMessages;
  msg := TCoverThreadMessage.Create;
  msg.typ := cmtRequest;
  msg.request := request;
  thread.messages.storeMessage(msg);
  result := nil;
end;

constructor TCoverLoaderAccess.create;
begin
  cache := TMapStringOwningObject.Create;
  thread := TCoverThread.Create();
  thread.OnTerminate := @threadTerminated;
  thread.OnImageReceived := @threadReceivedImage;
  emptyImageInfo := TCoverImageInfo.Create;
end;

procedure TCoverLoaderAccess.shutdownAndFree;
var msg: TCoverThreadMessage;
begin
  msg := TCoverThreadMessage.Create;
  msg.typ := cmtFree;
  thread.messages.storeMessage(msg);
end;

destructor TCoverLoaderAccess.Destroy;
begin
  cache.Free;
  emptyImageInfo.Free;
  inherited Destroy;
end;

procedure TCoverLoaderAccess.removeOldMessages;
var
  list: TFPList;
  i: Integer;
begin
  list:=thread.messages.openDirectMessageAccess;
  for i:=list.Count-1 downto 0 do
    if TCoverThreadMessage(list[i]).typ=cmtRequest then begin
      tobject(list[i]).free;
      list.Delete(i);
    end;
  thread.messages.closeDirectMessageAccess(list);
end;

procedure TCoverLoaderAccess.threadReceivedImage;
const MAX_IMAGES = 50;
      MIN_IMAGES = 30;
var
  i: Integer;
begin
  if cache.Count > MAX_IMAGES then begin
    while cache.Count > MIN_IMAGES do
      cache.Delete(0);
  end;

  thread.outputAccessSection.enter;
  try
    for i := 0 to high(thread.outputImageInfo) do
      cache.AddObject(thread.outputImageInfo[i].request.id, thread.outputImageInfo[i]);
    thread.outputImageInfo := nil;
  finally
    thread.outputAccessSection.leave;
  end;
  if assigned(OnImageReceived) then OnImageReceived(self);
end;

procedure TCoverLoaderAccess.threadTerminated(t: tobject);
begin
  ignore(t);
  free
end;

end.

