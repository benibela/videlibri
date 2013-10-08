unit libraryaccesstester;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, libraryParser;

type

  { TlibraryTesterForm }

  TlibraryTesterForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  libraryTesterForm: TlibraryTesterForm;

implementation

uses booklistreader, applicationconfig, internetaccess;
{ TlibraryTesterForm }

type

{ TTemplateAccountAccessTester }

 TTemplateAccountAccessTester = class(TTemplateAccountAccess)
  procedure init(apath, userID: string); override;
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
  t.init('', edit1.text);
  t.passWord:=edit2.Text;
  try
    t.connect(internet);
    t.updateAll;
    memo1.Lines.Clear;
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
    ListBox1.ItemIndex:=ListBox1.ItemIndex+1;
  end;
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
begin
  for i := 0 to libraryManager.count - 1 do
    ListBox1.Items.Add(libraryManager.getLibraryFromEnumeration(i).prettyNameLong);
end;

initialization
  {$I libraryaccesstester.lrs}

end.

