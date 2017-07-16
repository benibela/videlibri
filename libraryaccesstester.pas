unit libraryaccesstester;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Spin, TreeListView,
  libraryParser;

type

  { TlibraryTesterForm }

  TlibraryTesterForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    editSearchProp: TEdit;
    editTitle: TEdit;
    EditAutor: TEdit;
    EditUser: TEdit;
    editPass: TEdit;
    filter: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TreeListView1: TTreeListView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure TreeListView1CustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp; recordItem: TTreeListRecordItem;
      var defaultDraw: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  libraryTesterForm: TlibraryTesterForm;

implementation

uses booklistreader, applicationconfig, internetaccess, bbutils, Clipbrd, librarySearcher,math;
{ TlibraryTesterForm }


procedure testISBN;
const test: array[1..3] of string = (
  '346204567X', '3462045679', '978-3-462-04567-3'
);
var
  temp: TBook;
  i: Integer;
begin
  exit;
  i := 1;
  temp := tbook.create;
  while i <= high(test) do begin
    temp.isbn := test[i];
    if  temp.getNormalizedISBN(false, false) <> test[i+1] then
      raise Exception.Create('Failed: '+temp.isbn + ': ' + temp.getNormalizedISBN(false, false) +'<>' +test[i+1]);
    if  temp.getNormalizedISBN(false, true) <> test[i+2] then
      raise Exception.Create('Failed: '+temp.isbn + ': ' + temp.getNormalizedISBN(false, true) +'<>' +test[i+2]);
    i += 3;
  end;
  temp.free;
end;

var activeThreads, pendingThreads: integer;
const pendingLimit: integer = 50;
type

{ TTemplateAccountAccessTester }

 TTemplateAccountAccessTester = class(TTemplateAccountAccess)
  procedure init(apath, userID: string); override;
end;

 type TTestData = class
  lib: TLibrary;
  constructor create(thelib: TLibrary);
end;
type TTestThread = class(TThread)
  lib: TLibrary;
  row: TTreeListItem;
  fakeUser, fakePwd: string;
  title,author,moreSearchProp: string;
  resultSearch, resultAccount, resultHomepage: string;
  search, account, homepage: boolean;
  searchCount: integer;

  pending: boolean;
  constructor Create(arow: TTreeListItem; dosearch, doaccount, dohomepage: boolean; asearchCount: integer; atitle,anauthor,asearchprop, afakeUser, afakePwd: string);
  procedure Execute; override;
  procedure showResult;
  procedure checkPending;
end;

{ TTemplateAccountAccessTester }

procedure TTemplateAccountAccessTester.init(apath, userID: string);
begin
  self.path:='/tmp';
  self.user:=userID;
  ;
  DeleteFile('/tmp/test.history');
  DeleteFile('/tmp/test.current');
  fbooks:=TBookLists.create(self,'/tmp/test.history','/tmp/test.current');
  reader.books:=fbooks.currentUpdate;
end;

procedure TlibraryTesterForm.Button1Click(Sender: TObject);
var t: TTemplateAccountAccessTester;
  lib: TLibrary;
  i: Integer;
  internet: TInternetAccess;
begin
  internet := internetaccess.defaultInternetAccessClass.create();
  lib := libraryManager.getLibraryFromEnumeration(ListBox1.ItemIndex);
  t := TTemplateAccountAccessTester.create(lib);
  memo1.Lines.Clear;
  if (ComboBox1.ItemIndex > 0) <> lib.segregatedAccounts then memo1.lines.add('segregation mismatch');
  t.init('', edit1.text);
  t.passWord:=edit2.Text;
  t.accountType:=ComboBox1.ItemIndex;
  try
    t.connect(internet);
    t.updateAll;
    for i := 0 to t.books.currentUpdate.Count-1do
      memo1.Lines.Add(t.books.currentUpdate[i].toLimitString());
  finally
    t.free;

  end;
end;

procedure TlibraryTesterForm.Button2Click(Sender: TObject);
var
  Message: String;
begin
  while ListBox1.ItemIndex < ListBox1.Items.Count - 1 do begin
    if  (filter.Text = '') or (pos(filter.Text, libraryManager.getLibraryFromEnumeration(ListBox1.ItemIndex).template.name) > 0) then begin
      try
        button1.Click;
        Message := 'PASS!';
      except
        on e: EBookListReader do begin
          message := e.Message;;
        end;
      end;
      memo1.Lines.Add(ListBox1.Items[ListBox1.ItemIndex] +': '+ trim(Message));
      Application.ProcessMessages;
    end;
    ListBox1.ItemIndex:=ListBox1.ItemIndex+1;
  end;
end;

procedure TlibraryTesterForm.Button3Click(Sender: TObject);
var
  i: Integer;
begin
  memo1.Lines.Clear;
  for i := 0 to libraryManager.count - 1 do
    Memo1.Lines.Add(libraryManager[i].id+':§: '+libraryManager[i].prettyNameShort+':§: '+libraryManager[i].homepageCatalogue);
end;

procedure TlibraryTesterForm.Button4Click(Sender: TObject);
var
  i: Integer;
begin
  pendingLimit := SpinEdit2.Value;
  for i := 0 to TreeListView1.Items.Count - 1 do
    if CheckBox1.Checked or TreeListView1.Items[i].Selected then
      TTestThread.Create(TreeListView1.Items[i],CheckBox2.Checked,CheckBox3.Checked, CheckBox4.Checked, SpinEdit1.Value,
                         editTitle.Text, EditAutor.Text, editSearchProp.Text,
                         EditUser.Text,editPass.text);
  Caption := 'Active Threads: ' + IntToStr(activeThreads) + ' Pending Threads: ' + IntToStr(pendingThreads);
end;

procedure TlibraryTesterForm.Button5Click(Sender: TObject);
var
  i: Integer;
  tocp: String;
begin
  tocp := '';
  for i := 0 to TreeListView1.Items.Count - 1 do
    if TreeListView1.Items[i].Selected then
      tocp += TreeListView1.Items[i].RecordItemsText[0] + ' ' +TreeListView1.Items[i].RecordItemsText[1] + LineEnding
              + TTestData(TreeListView1.Items[i].data.obj).lib.homepageCatalogue + LineEnding
              + TreeListView1.Items[i].RecordItemsText[2] +LineEnding+TreeListView1.Items[i].RecordItemsText[3] + LineEnding+TreeListView1.Items[i].RecordItemsText[4] + LineEnding+LineEnding;
  Clipboard.AsText := tocp;
end;

procedure TlibraryTesterForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TlibraryTesterForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;



procedure TlibraryTesterForm.FormCreate(Sender: TObject);
var
  i: Integer;
  lib: TLibrary;
begin
  testISBN;
  TreeListView1.BeginUpdate;
  for i := 0 to libraryManager.count - 1 do begin
    lib := libraryManager.getLibraryFromEnumeration(i);
    ListBox1.Items.Add(lib.prettyNameLong);
    TreeListView1.Items.Add([lib.prettyCountryState, lib.prettyNameLong]).data.obj := TTestData.create(lib);
    TreeListView1.Items[TreeListView1.Items.Count-1].RecordItemsText[5] := lib.template.name;
  end;
  TreeListView1.EndUpdate;
  TreeListView1.RowHeight := TreeListView1.RowHeight + 5;
  TreeListView1.UpdateScrollSize;
end;

procedure TlibraryTesterForm.TreeListView1CustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp;
  recordItem: TTreeListRecordItem; var defaultDraw: Boolean);
begin
  if eventTyp_cdet <> cdetPrePaint then exit ;
  if not recordItem.Parent.SeemsSelected then begin
    if strBeginsWith(recordItem.Text, '1-') then begin
      TreeListView1.canvas.Brush.Color := clYellow;
    end else if strBeginsWith(recordItem.Text, '2-') then begin
      TreeListView1.canvas.Brush.Color := clRed;
    end else exit;
    TreeListView1.canvas.Brush.Style := bsSolid;
    TreeListView1.Canvas.FillRect(TreeListView1.DrawingRecordItemRect);
  end;
end;


constructor TTestData.create(thelib: TLibrary);
begin
  lib := thelib;
end;

constructor TTestThread.Create(arow: TTreeListItem; dosearch, doaccount, dohomepage: boolean; asearchCount: integer; atitle, anauthor,
  asearchprop, afakeUser, afakePwd: string);
begin
  inherited Create(false);
  row := arow;
  lib := TTestData(arow.data.obj).lib;
  resultSearch := row.RecordItemsText[2];
  resultAccount := row.RecordItemsText[3];
  resultHomepage := row.RecordItemsText[4];
  fakeUser := afakeUser;
  fakePwd := afakePwd;
  resultAccount := '';
  search := dosearch;
  account := doaccount;
  homepage := dohomepage;
  title := atitle;
  author := anauthor;
  moreSearchProp := asearchprop;
  searchCount := asearchCount;

  pending := activeThreads > pendingLimit;
  if pending then inc(pendingThreads)
  else inc(activeThreads);
end;

procedure TTestThread.Execute;
var t: TTemplateAccountAccessTester;
  i, acctype: Integer;
  internet: TInternetAccess;
  searcher: TLibrarySearcher;
  critSection: TRTLCriticalSection;
begin
  while pending do begin
    sleep(500);
    ReadBarrier;
    if activeThreads < pendingLimit then //sychronize is heavy. if we synchronize all the ckecking, the active thread cannot use synchronize to quit
      Synchronize(@checkPending);
  end;
  if account then begin
    try
      resultAccount := '';
      for acctype := 1 to ifthen(lib.segregatedAccounts, 2, 1) do begin
        internet := internetaccess.defaultInternetAccessClass.create();
        t := TTemplateAccountAccessTester.create(lib);
        t.init('', fakeUser);
        t.accountType:=acctype;
        t.passWord:=fakePwd;


        try
          t.connect(internet);
          t.updateAll;
          for i := 0 to t.books.currentUpdate.Count-1do
            resultAccount += t.books.currentUpdate[i].toLimitString();
        except
          on e: ELoginException do
            resultAccount += '0-'+e.ClassName +': '+ e.Message + ' '+resultAccount;
          on e: EBookListReader do
            resultAccount += '1-'+e.ClassName +': '+ e.Message + ' '+resultAccount;
          on e: Exception do
            resultAccount += '2-' + e.ClassName +': '+ e.Message + ' '+resultAccount;
        end;

        t.free;
      end;
    except
      on e: exception do
        resultAccount := '2-UNHANDLED EXCEPTION: '+e.Message;
    end;
  end;
  if search then begin
    try
      InitCriticalSection(critSection);
      searcher := TLibrarySearcher.create(lib.template);
      Searcher.bookListReader.bookAccessSection:=@critSection;
      searcher.addLibrary(lib);

      try
        searcher.SearchOptions.title := title;
        searcher.SearchOptions.author := author;
        if moreSearchProp <> '' then begin
          searcher.SearchOptions.setProperty(strBefore(moreSearchProp, '='), strAfter(moreSearchProp, '=') );
        end;
        searcher.connect;
        searcher.search;
        if searcher.SearchNextPageAvailable then
          searcher.searchNext;
        resultSearch := inttostr(searcher.SearchResult.Count) + '/'+ inttostr(searcher.SearchResultCount) +': ';
        for i := 0 to min(searchCount, searcher.SearchResult.Count - 1) do begin
          searcher.details(searcher.SearchResult[i]);
          if i <> 0 then resultSearch += ', ';
          resultSearch += searcher.SearchResult[i].toSimpleString();
        end;
        if searcher.SearchResultCount = 0 then resultSearch := '1-' + resultSearch;
      except
        on e: EBookListReader do
          resultSearch := '1-'+e.ClassName +': '+ e.Message;
        on e: Exception do
          resultSearch := '2-' + e.ClassName +': '+ e.Message;
      end;
      searcher.free;
      DoneCriticalsection(critSection);
    except
      on e: exception do
        resultSearch := '2-UNHANDLED EXCEPTION: '+e.Message;
    end;
  end;
  if homepage then begin
    try
      internetaccess.defaultInternetAccessClass.create().get(lib.homepageBase);
      resultHomepage := 'ok';
    except
      on e: EInternetException do
        resultHomepage := '2- '+ e.Message + ' '+e.details;
    end
  end;
  Synchronize(@showResult);
end;

procedure TTestThread.showResult;
begin
  if search then
    row.RecordItemsText[2] := resultSearch;
  if account then
    row.RecordItemsText[3] := resultAccount;
  if homepage then
    row.RecordItemsText[4] := resultHomepage;
  dec(activeThreads);
  (row.TreeListView.Owner as tform).Caption := 'Active Threads: ' + IntToStr(activeThreads) + ' Pending Threads: ' + IntToStr(pendingThreads);
end;

procedure TTestThread.checkPending;
begin
  if activeThreads < pendingLimit then begin
    dec(pendingThreads);
    inc(activeThreads);
    pending := false;
    (row.TreeListView.Owner as tform).Caption := 'Active Threads: ' + IntToStr(activeThreads) + ' Pending Threads: ' + IntToStr(pendingThreads);
  end;
end;

initialization
  {$I libraryaccesstester.lrs}

end.

