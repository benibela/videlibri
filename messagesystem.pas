unit messagesystem;

{$mode objfpc}

interface

uses
  Classes, SysUtils; 

//there was a reason that this is crap...
type TMessageSystem = class
private
  messageAccessSection: TRTLCriticalSection;
  list: TFPList;
public
  //synchronized methodes
  procedure storeMessage(mes: TObject);
  function retrieveMessageOrNil:TObject; //returns nil if no message exists
  function retrieveLatestMessageOrNil:TObject; //returns nil if no message exists
  function waitForMessage:TObject;
  function existsMessage: boolean;

  procedure removeAndFreeAll;

  function openDirectMessageAccess: TFPList; //will block every access until close
  procedure closeDirectMessageAccess(dlist: TFPList);


  //not synchronized
  constructor create;
  destructor destroy;override;
end;
implementation


{ TMessageSystem }

procedure TMessageSystem.storeMessage(mes: TObject);
begin
  if mes=nil then raise exception.Create('Tried to store not existing message in the message queue');
  EnterCriticalSection(messageAccessSection);
  list.add(mes);
  LeaveCriticalSection(messageAccessSection);
end;

function TMessageSystem.retrieveMessageOrNil: TObject;
begin
  EnterCriticalSection(messageAccessSection);
  if list.Count=0 then result:=nil
  else begin
    result:=tobject(list[0]);
    list.delete(0);
  end;
  LeaveCriticalSection(messageAccessSection);
end;

function TMessageSystem.retrieveLatestMessageOrNil: TObject;
begin
  EnterCriticalSection(messageAccessSection);
  if list.Count=0 then result:=nil
  else begin
    result:=tobject(list[list.count-1]);
    list.delete(list.count-1);
  end;
  LeaveCriticalSection(messageAccessSection);
end;

function TMessageSystem.waitForMessage: TObject;
begin
  Result:=nil;
  while result = nil do begin
    while not existsMessage do sleep(20);
    result:=retrieveMessageOrNil; //list.count=0 is possible
  end;
end;

function TMessageSystem.existsMessage: boolean;
begin
  result:=list.Count>0;
  ReadBarrier;
end;

procedure TMessageSystem.removeAndFreeAll;
var i:longint;
begin
  EnterCriticalSection(messageAccessSection);
  for i:=0 to list.Count-1 do
    tobject(list[i]).free;
  list.clear;
  LeaveCriticalSection(messageAccessSection);
end;

function TMessageSystem.openDirectMessageAccess: TFPList;
begin
  EnterCriticalSection(messageAccessSection);
  Result:=list;
end;

procedure TMessageSystem.closeDirectMessageAccess(dlist: TFPList);
begin
  LeaveCriticalSection(messageAccessSection);
  if self.list<>dlist then raise Exception.Create('Invalid List');
end;

constructor TMessageSystem.create;
begin
  InitCriticalSection(messageAccessSection);
  list:=TFPList.Create;
end;

destructor TMessageSystem.destroy;
begin
  DoneCriticalsection(messageAccessSection);
  list.free;
  inherited destroy;
end;


end.

